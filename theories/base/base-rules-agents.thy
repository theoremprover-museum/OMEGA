;;; -*- syntax: common-lisp; package: omega; base: 10; mode: keim -*-

(in-package :omega)
(th~require-completely 'base)

(eval-when (compile)
           (error "This file should not be compiled."))


(dflt~defmatrix andi rules		
   (agents (c-predicate (for Conjunction)
                        (uses )
			(level 1)
                        (definition (logic~conjunction-p Conjunction)))
           (c-predicate (for Conjunction)
                        (uses LConj)
			(level 1)
                        (definition (pred1 Conjunction LConj)))
           (c-predicate (for Conjunction)
                        (uses RConj)
			(level 1)
                        (definition (pred2 Conjunction RConj)))
           (c-predicate (for Conjunction)
                        (uses LConj RConj)
			(level 1)
                        (definition (and (pred1 Conjunction LConj)
                                         (pred2 Conjunction RConj))))
           (s-predicate (for RConj)
                        (uses Conjunction)
			(level 1)
                        (definition (pred2 Conjunction RConj)))
	   (s-predicate (for LConj)
                        (uses Conjunction)
			(level 1)
                        (definition (pred1 Conjunction LConj))
                        (help "Predicate for the left conjunct of a given conjunction.")))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))



(dflt~defmatrix weaken rules
   (agents (c-predicate (for lowerline)
                        (uses upperline)
			(level 1)
                        (definition (pred1 lowerline upperline)))
           (c-predicate (for lowerline)
			(level 1)
                        (definition (pred2 lowerline)))
           (s-predicate (for upperline)
                        (uses lowerline)
			(level 1)
                        (definition (pred1 lowerline upperline)))
	   (s-predicate (for upperline)
			(level 1)
                        (definition (pred3 upperline))))
   (predicates
    (pred1 (a1 a2)
	   (progn
	     (format t "~%Pred1 is running on arg ~A and ~A" a1 a2)
	     (term~alpha-equal a1 a2)))
    (pred2 (line1)
	   (progn
	     (format t "~%Pred2 is running on arg ~A" line1)
	     (find-if #'(lambda (:protect x) (pred1 line1 (node~formula x)))
		      (pds~node-supports (:node line1)))))
    (pred3 (line2)
	   (progn
	     (format t "~%Pred3 is running on arg ~A" line2)
	     (find-if #'(lambda (:protect x) (pred1 (node~formula x) line2))
		      (pds~open-nodes omega*current-proof-plan))))))



(dflt~defmatrix orir rules
   (agents (c-predicate (for disjunction)
                        (uses )
			(level 1)
                        (definition (logic~disjunction-p disjunction)))
           (c-predicate (for disjunction)
                        (uses rdisj)
			(level 1)
                        (definition (pred1 disjunction rdisj)))
           (c-predicate (for disjunction)
                        (uses rdisj par)
			(level 1)
                        (definition (pred2 disjunction rdisj (:param par))))
           (s-predicate (for rdisj)
                        (uses disjunction)
			(level 1)
                        (definition (pred1 disjunction rdisj)))
	   (function    (for par)
			(level 1)
                        (uses disjunction)
			(level 1)
                        (definition (first (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis rdis) (and (logic~disjunction-p dis)
			   (data~equal (second (data~appl-arguments dis)) rdis)))
    (pred2 (dis rdis par) (and (logic~disjunction-p disjunction)
			       (data~equal (second (data~appl-arguments dis)) rdis)
			       (data~equal (first (data~appl-arguments dis)) par)))))


(dflt~defmatrix oril rules
   (agents (c-predicate (for disjunction)
                        (uses )
			(level 1)			
                        (definition (logic~disjunction-p disjunction)))
           (c-predicate (for disjunction)
                        (uses ldisj)
			(level 1)
                        (definition (pred1 disjunction ldisj)))
           (c-predicate (for disjunction)
                        (uses ldisj par)
			(level 1)
                        (definition (pred2 disjunction ldisj (:param par))))
           (s-predicate (for ldisj)
                        (uses disjunction)
			(level 1)
                        (definition (pred1 disjunction ldisj)))
	   (function    (for par)
                        (uses disjunction)
			(level 1)
                        (definition (second (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis ldis) (and (logic~disjunction-p dis)
			   (data~equal (first (data~appl-arguments dis)) ldis)))
    (pred2 (dis ldis par) (and (logic~disjunction-p ldis)
			       (data~equal (first (data~appl-arguments dis)) ldis)
			       (data~equal (second (data~appl-arguments dis)) par)))))


(dflt~defmatrix ore rules
   (agents (s-predicate (for disjunction)
                        (uses )
			(level 1)
                        (definition (logic~disjunction-p disjunction)))
	   (c-predicate (for goal)
			(uses )
			(level 3)
			(definition (pds~find-node-support (:node goal) #'logic~disjunction-p)))))  ;;; wie geht das hier


(dflt~defmatrix andel rules
   (agents (s-predicate (for conjunction)
                        (uses )
			(level 1)
                        (definition (logic~conjunction-p conjunction)))
           (s-predicate (for conjunction)
                        (uses lconj)
			(level 1)
                        (definition (pred1 conjunction lconj)))
	   (c-predicate (for lconj)
			(level 1)
                        (uses conjunction)
                        (definition (pred1 conjunction lconj))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))))


(dflt~defmatrix ander rules	
   (agents (s-predicate (for conjunction)
                        (uses )
			(level 1)
                        (definition (logic~conjunction-p conjunction)))
           (s-predicate (for conjunction)
                        (uses rconj)
			(level 1)
                        (definition (pred1 conjunction rconj)))
	   (c-predicate (for rconj)
			(level 1)
                        (uses conjunction)
                        (definition (pred1 conjunction rconj))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))


(dflt~defmatrix impi rules	
   (agents (c-predicate (for implication)
                        (uses )
			(level 1)
                        (definition (logic~implication-p implication)))))


(dflt~defmatrix impe rules	
   (agents (s-predicate (for implication)
                        (uses )
			(level 1)
                        (definition (logic~implication-p implication)))
           (s-predicate (for implication)
                        (uses antecedent)
			(level 1)
                        (definition (pred1 implication antecedent)))
           (s-predicate (for implication)
                        (uses succedent)
			(level 1)
                        (definition (pred2 implication succedent)))
           (s-predicate (for implication)
                        (uses antecedent succedent)
			(level 1)
                        (definition (and (pred1 implication antecedent)
                                         (pred2 implication succedent))))
           (c-predicate (for succedent)
                        (uses implication)
			(level 1)
                        (definition (pred2 implication succedent)))
	   (s-predicate (for antecedent)
                        (uses implication)
			(level 1)
                        (definition (pred1 implication antecedent))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~implication-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~implication-p a1)
                (data~equal a2 (second (data~appl-arguments a1)))))))


(dflt~defmatrix noti rules	
   (agents (s-predicate (for falsity)
                        (uses )
			(level 1)
                        (definition (logic~falsity-constant-p falsity)))
           (s-predicate (for falsity)   ;;; wie geht das hier ;;; aber das ist
						;;; doch eigentlich quatsch hier
                        (uses negation)
			(level 3)
                        (definition (pds~find-node-support (:node negation) #'(lambda (:protect x) (keim~equal x falsity))))) 
	   (c-predicate (for negation)
                        (uses )
			(level 1)
                        (definition (logic~negation-p negation)))))



(dflt~defmatrix note rules	
   (agents (s-predicate (for negation)
                        (uses )
			(level 1)
                        (definition (logic~negation-p negation)))
	   (s-predicate (for negation)
                        (uses line)
			(level 1)
                        (definition (pred1 negation line)))
	   (c-predicate (for falsehood)
			(uses )
			(level 1)
			(definition (logic~falsity-constant-p falsehood)))
	   (s-predicate (for line)
                        (uses negation)
			(level 1)
                        (definition (pred1 negation line))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~negation-p a1)
                (data~equal a2 (first (data~appl-arguments a1)))))))


(dflt~defmatrix falsee rules	
   (agents (s-predicate (for falsehood)
			(uses )
			(level 1)
			(definition (logic~falsity-constant-p falsehood)))))


(defun orules=matching-term (term1 term2)
  (multiple-value-bind (ok subst)
      (term~alpha-match term1 term2)
    (when (and ok (= 1 (length (subst~domain subst))))
      (car (subst~codomain subst)))))


(dflt~defmatrix existsi rules	
   (agents (c-predicate (for exists-line)
                        (uses )
			(level 1)
                        (definition (logic~existential-quantification-p exists-line)))
           (c-predicate (for exists-line)
                        (uses prem)
			(level 1)
                        (definition (pred1 exists-line prem)))
	   (c-predicate (for exists-line)
                        (uses prem witness)
			(level 1)
                        (definition (pred2 exists-line prem (:param witness))))
	   (c-predicate (for exists-line)
                        (uses prem witness positionlist)
			(level 5)
                        (definition (pred3 exists-line prem (:param witness) (:param positionlist))))
	   (s-predicate (for prem)
                        (uses exists-line)
			(level 1)
                        (definition (pred1 exists-line prem)))
	   (c-predicate (for prem)
                        (uses exists-line witness)
			(level 1)
                        (definition (pred2 exists-line prem (:param witness))))
	   (c-predicate (for prem )
                        (uses exists-line witness positionlist)
			(level 5)
                        (definition (pred3 exists-line prem (:param witness) (:param positionlist))))
	   (function (for witness)
		     (uses prem)
		     (level 3)
		     (definition (func1 prem)))  ;;erste Konstante suchen -- macht zwar
						 ;;nicht viel Sinn ..
	   (function (for witness)
		     (uses exists-line)
		     (level 1)
		     (definition (func2 exists-line)))   ;; beliebige neue Konstante berechnen
	   (function (for witness)
		     (uses exists-line prem)
		     (level 5)
		     (definition (func3 exists-line prem)))   ;; exists-var liefert positionen, dort
						              ;; steht term
	   (function (for positionlist)
		     (uses exists-line)
		     (level 5)
		     (definition (func4 exists-line)))   ;; ;; exists-var liefert positionen
	   (function (for positionlist)
		     (uses prem witness)
		     (level 3)
		     (definition (func5 prem (:param witness))))  ;; suche positionen von witness
	   ;; in prem
	   )
   (predicates
    (pred1 (a1 a2)
	   (and
	    (logic~existential-quantification-p a1)
	    (term~alpha-match a2 (logic~quantification-scope a1))))
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
	    (every #'(lambda (:protect pos)
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
						 (pds~environment omega*current-proof-plan)))
    (func3 (a1 a2)
	   (when (logic~existential-quantification-p a1)
	     (orules=matching-term (logic~quantification-scope a1) a2)))
    (func4 (a1)
	   (data~positions (logic~quantification-scope a1)
			   #'(lambda (:protect x) (data~equal x (logic~quantification-bound-variable
							a1)))))
    (func5 (a1 a2)
	   (data~positions a1 #'(lambda (:protect x) (data~equal x a2))))))
	   

		      

(dflt~defmatrix existse rules	
   (agents (s-predicate (for exline)
                        (uses )
			(level 1)
                        (definition (logic~existential-quantification-p exline)))
           (s-predicate (for exline)
                        (uses subgoal)
			(level 5)
                        (definition (pred1 exline subgoal)))
	   (s-predicate (for exline)
                        (uses conc)
			(level 5)
                        (definition (pred1 exline conc)))
	   (c-predicate (for conc)
                        (uses )
			(level 3)
                        (definition (pred2 conc)))
	   (c-predicate (for conc)
                        (uses exline)
			(level 3)
                        (definition (pred3 exline conc)))
	   (c-predicate (for conc)
                        (uses subgoal)
			(level 1)
                        (definition  (and (not (equal (:node conc) (:node subgoal)))
					  (data~equal conc subgoal))))
	   (s-predicate (for subgoal)
                        (uses )
			(level 3)
                        (definition (pred2 subgoal)))
           (s-predicate (for subgoal)
                        (uses conc)
			(level 1)
                        (definition (and (not (equal (:node conc) (:node subgoal)))
                                         (data~equal conc subgoal))))
           (s-predicate (for subgoal)
                        (uses exline)
			(level 3)
                        (definition (pred3 exline subgoal)))
	   (function (for parameter)
		     (uses exline)
		     (level 1)
		     (definition (func0 exline))) 
	   (function (for parameter)
		     (uses exline conc)
		     (level 10)
		     (definition (func1 exline conc)))  ;;suche matchbarer exlinescope in
						 ;;supports schlage ergebnis matching vor
	   
	   (function (for parameter)
		     (uses exline subgoal)
		     (level 10)
		     (definition (func1 exline subgoal))) ;;suche matchbarer exlinescope in
						 ;;supports schlage ergebnis matching vor
	   
	   )
   (predicates
    (pred1 (exline subgoal)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support (:node subgoal) #'(lambda (:protect x)
						       (term~alpha-match
							(logic~quantification-scope exline)
							x)))))
    (pred2 (conc)
	   (pds~find-node-support  (:node conc) #'(lambda (:protect x)
						    (logic~existential-quantification-p x))))
    (pred3 (conc exline)
	   (and
	    (logic~existential-quantification-p exline)
	    (pds~find-node-support  (:node conc) #'(lambda (:protect x)
						     (data~equal x exline)))))
    (func0 (exline) (when (logic~existential-quantification-p exline)
		      (term~generate-term-primitive-with-new-name 
		       'a (term~type (logic~quantification-bound-variable exline))
		       'term+constant (pds~environment omega*current-proof-plan))))
    (func1 (exline goal)
	   (when
	       (logic~existential-quantification-p exline)
	     (orules=matching-term
	      (logic~quantification-scope exline)
	      (or (car 
		   (pds~find-node-support (:node goal) #'(lambda (:protect x)
							   (and (term~p x)
								(orules=matching-term  
								 (logic~quantification-scope exline)
								 x)))))
		  (logic~negation-constant)))))))  ;schwachsinn muss spaeter weg





(dflt~defmatrix foralle rules
   (agents (s-predicate (for univ-line)
                        (uses )
			(level 1)
                        (definition (logic~universal-quantification-p univ-line)))
           (s-predicate (for univ-line)
                        (uses line)
			(level 1)
                        (definition (pred1 univ-line line)))
           (s-predicate (for univ-line)
                        (uses line term)
			(level 3)
                        (definition (pred2 univ-line line (:param term))))
           (c-predicate (for line)
                        (uses univ-line)
			(level 1)
                        (definition (pred1 univ-line line)))
	   (c-predicate (for line)
                        (uses univ-line term)
			(level 3)			
                        (definition (pred2 univ-line line (:param term))))
	   (function    (for term)
                        (uses line)
			(level 3)		
                        (definition (func1 line))) ;; erste Konstante suchen
	   (function    (for term)
                        (uses univ-line)
			(level 1)		
                        (definition (func2 univ-line))) ;; neue Konstante vorschlagen
	   (function    (for term)
                        (uses univ-line line)
			(level 1)		
                        (definition (func3 univ-line line)))) ;; exakte Konstante berechnen
   (predicates
    (pred1 (univ-line line)
	   (and
	    (logic~universal-quantification-p univ-line)
	    (term~alpha-match (logic~quantification-scope univ-line) line)))
    
    (pred2 (univ-line line term)
	   (format t "hallo")
	   (and
	    (logic~universal-quantification-p univ-line)
	    (orules=matching-term (logic~quantification-scope univ-line) line)
	    (data~equal (orules=matching-term (logic~quantification-scope univ-line) line)
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
						  (pds~environment omega*current-proof-plan))))
    (func3 (univ-line line)
	   (when (pred1 univ-line line)
	     (orules=matching-term (logic~quantification-scope univ-line) line)))))


(dflt~defmatrix foralli rules
   (agents (c-predicate (for univ-line)
                        (uses )
			(level 1)		
                        (definition (logic~universal-quantification-p univ-line)))
           (c-predicate (for univ-line)
                        (uses line)
			(level 1)		
                        (definition (pred1 univ-line line)))
           (c-predicate (for univ-line)
                        (uses line parameter)
			(level 3)		
                        (definition (pred2 univ-line line (:param parameter))))
           (s-predicate (for line)
                        (uses univ-line)
			(level 1)		
                        (definition (pred1 univ-line line)))
	   (s-predicate (for line)
                        (uses univ-line parameter)
			(level 3)		
                        (definition (pred2 univ-line line (:param parameter))))
	   (function    (for parameter)
                        (uses line)
			(level 3)		
                        (definition (func1 line))) ;; erste constante suchen
	   (function    (for parameter)
                        (uses univ-line)
			(level 1)		
                        (definition (func2 univ-line))) ;; neue Konstante vorschlagen
	   (function    (for parameter)
                        (uses univ-line line)
			(level 1)		
                        (definition (func3 univ-line line)))) ;; exakte Konstante berechnen
   (predicates
    (pred1 (univ-line line)
	   (and
	    (logic~universal-quantification-p univ-line)
	    (orules=matching-term (logic~quantification-scope univ-line) line)))
    
    (pred2 (univ-line line parameter)
	   (and
	    (pred1 univ-line line)
	    (data~equal (orules=matching-term (logic~quantification-scope univ-line) line)
			parameter)))
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
						  (pds~environment omega*current-proof-plan))))
    (func3 (univ-line line)
	  (when (pred1 univ-line line)
	    (orules=matching-term (logic~quantification-scope univ-line) line)))))


(dflt~defmatrix lambda rules
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



(dflt~defmatrix defn-expand rules
   (agents (s-predicate (for line)
                        (uses )
			(level 3)		
                        (definition (orules=contained-definition line)))
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
		     (definition (orules=contained-definition line)))
           (function (for definition)
		     (uses line position)
		     (level 30)
		     (definition (func1 line (:param position))))
	   (function (for position)
		     (uses line)
		     (level 3)
		     (definition (car (data~substruct-positions (orules=contained-definition
							    line)
							   line))))
	   (function (for position)
		     (uses line definition)
		     (level 3)
		     (definition (car (pred1 line (:param definition))))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (:protect x)
				     (and (term~constant-p x)
					  (keim~equal (th~definition-constant definition)
						      x)))))
    (pred2 (line definition position)
	   (find position (pred1 line definition)))
    (func1 (line position)
	   (when (find position (data~positions line #'term-constant-p))
	     (find-if
	      #'(lambda (:protect x) (keim~equal (data~struct-at-position line position)
					(th~definition-constant x)))
	      (th~definitions-recursed (prob~theory omega*current-proof-plan)))))))
    
    
(dflt~defmatrix defn-contract rules
   (agents (c-predicate (for line)
                        (uses )
			(level 3)
                        (definition (orules=contained-definition line)))
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
		     (definition (orules=contained-definition line)))
           (function (for definition)
		     (uses line pos)
		     (level 30)
		     (definition (func1 line (:param pos))))
	   (function (for pos)
		     (uses line)
		     (level 3)
		     (definition (car (data~substruct-positions (orules=contained-definition
								 line)
								line))))
	   (function (for pos)
		     (uses line definition)
		     (level 3)
		     (definition (car (pred1 line (:param definition))))))
   (predicates
    (pred1 (line definition)
	    (data~positions line #'(lambda (:protect x)
				     (and (term~constant-p x)
					  (keim~equal (th~definition-constant definition)
						      x)))))
    (pred2 (line definition position)
	   (find position (pred1 line definition)))
    (func1 (line position)
	   (when (find position (data~positions line #'term-constant-p))
	     (find-if
	      #'(lambda (:protect x) (keim~equal (data~struct-at-position line position)
					(th~definition-constant x)))
	      (th~definitions-recursed (prob~theory omega*current-proof-plan)))))))


(defun orules=ext-check (all-term eq-term)
  (when (and (logic~equality-p eq-term)
	     (logic~universal-quantification-p all-term))
    (let* ((left (car (data~appl-arguments eq-term)))
	   (right (cadr (data~appl-arguments eq-term)))
	   (eq (data~appl-function eq-term))
	   (bound (logic~quantification-bound-variable all-term))
	   (scope (logic~quantification-scope all-term))
	   (new-left (data~appl-create left (list bound)))
	   (new-right (data~appl-create right (list bound))))
      (and (logic~equality-p scope)
	   (data~equal (car (data~appl-arguments scope)) new-left)
	   (data~equal (cadr (data~appl-arguments scope)) new-right)))))



(dflt~defmatrix exte rules
   (agents (c-predicate (for lowerline)
                        (uses )
			(level 1)
                        (definition (logic~equality-p lowerline)))
           (c-predicate (for lowerline)
                        (uses upperline)
			(level 2)
                        (definition (orules=ext-check upperline lowerline)))
	   (s-predicate (for upperline)
                        (uses )
			(level 1)
                        (definition (logic~universal-quantification-p upperline)))
           (s-predicate (for upperline)
                        (uses lowerline)
			(level 2)
                        (definition (orules=ext-check upperline lowerline)))))
				       
	   

(dflt~defmatrix exti rules
   (agents (c-predicate (for lowerline)
                        (uses )
			(level 1)
                        (definition (logic~existential-quantification-p lowerline)))
           (c-predicate (for lowerline)
                        (uses upperline)
			(level 2)
                        (definition (orules=ext-check upperline lowerline)))
	   (s-predicate (for upperline)
                        (uses )
			(level 1)
                        (definition (logic~equality-p upperline)))
           (s-predicate (for upperline)
                        (uses lowerline)
			(level 2)
                        (definition (orules=ext-check upperline lowerline)))))



(dflt~defmatrix exti-neg rules
   (agents (c-predicate (for lowerline)
                        (uses )
			(level 1)
                        (definition (pred1 lowerline)))
           (c-predicate (for lowerline)
                        (uses upperline)
			(level 2)
                        (definition (and (pred1 lowerline) (pred2 upperline)
					 (orules=ext-check (car (data~appl-arguments lowerline))
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
					 (orules=ext-check (logic~quantification-scope
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




