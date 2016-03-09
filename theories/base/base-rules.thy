;;; -*- syntax: common-lisp; package: omega; base: 10; mode: keim -*-

;;;
;;; Please look into testreport.txt for a rule's status. Nota all rules
;;; have been tested so far...
;;;

;;;
;;; If you add a rule, please insert its name into the testreport file.
;;;

(in-package :omega)

(eval-when (compile)
           (error "This file should not be compiled."))

#-logic-new(load "/project/omega/omega-3/theories/base/generic-fix-rules.lisp")

;;;LC: Convention for rule naming:
;; rule-c: for the checking rule
;; rule-a: for the rule application, given an open conclusion and all the premises
;; rule-b: backward application of the rule, given an open conclusion
;; rule-f: forward application of the rule, given all the premises
;; rule-si: a side-ward application of the rule.
;; NOTE: Sideconditions have two purposes:
;; 1. Restriction the rule application: Use only support nodes as premises
;; 2. Checking the correctness of the rule application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun orules=check-node-on-argument (node check argnumber)
  (declare (edited  "18-JUL-1997")
	   (authors Sorge)
	   (input   "A node and two functions.")
	   (effect   "Checks whether the formula is T with CHECK and then searches for"
		    "a node in the supports of NODE, that is equal to the argument specified"
		    "by the argnumber-function.")
	   (values  "The supporting node or (COM~~UNSPECIFIED)."))
  (if (and (pdsn~p node) (funcall check (node~formula node)))
      (pds~find-node-support
       node
       #'(lambda (p)
	   (data~equal p (funcall argnumber (data~appl-arguments (node~formula node))))))
    (oc~nil-argument)))
  

;;; the Weaken rule:

(infer~defrule weaken
	       (outline-mappings (((closed closed) weaken-c)
				  ((existent existent) weaken-a)))
	       (help "The Weakening rule. Takes an assumption and increases its hypotheses."))

(rule~defrule weaken-c weaken (in base)
 (declarations 
  (meta-variables (A O)))
 (conclusion (C () A))
 (premises (P () A))
 (description "Checking an application of the Weaken rule that Takes an assumption and
 increases its hypotheses."))

(rule~defrule weaken-a weaken (in base)
 (declarations 
  (meta-variables (A O)))
 (conclusion (C () A))
 (premises (P () A))
 (sideconditions
   (pds~node-supported-by-p C P))
 (description "Justifying a node by another with the same formula."))

(com~defcommand weaken
  (argnames lowerline upperline)
  (argtypes ndline ndline)
  (arghelps "Line to justify" "Already-derived line")
  (function orules=weaken)
  (frag-cats rules structural)
  (defaults orules=weaken-defaults)
  (log-p T)
  (help "Justify a line from an identical earlier-derived line."))

(defun orules=weaken (lower upper)
  (infer~compute-outline 'weaken (list lower upper) nil))

(defun orules=weaken-defaults (lower upper)
  (if (and lower (com~specified-arg-p lower))
      (list lower
	    (pds~find-node-support lower
				   #'(lambda (p) (data~equal-p (node~formula lower) p))))
    (list (oc~default-current-planline)
	  (com~unspecified))))

;;; the Disjunction Introduction Right rule:
(infer~defrule orir
	       (outline-mappings (((closed closed) orir-c)
				  ((existent existent) orir-a)
				  ((nonexistent existent) orir-f)
				  ((existent nonexistent) orir-b)))
	       (parameter-types term)
	       (help "The OR-Introduction right rule."))


(rule~defrule orir-c orir (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Checking an application of the Disjunction Introduction Right rule."))

(rule~defrule orir-a orir (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Justifying a disjunction by its right disjunct."))

(rule~defrule orir-f orir (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Deducing a disjunction from its right disjunct."))

(rule~defrule orir-b orir (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Reducing the proof of a disjunction to the proof of its right disjunct."))

(com~defcommand orir
  (argnames disjunction rdisj par)
  (argtypes ndline ndline term)
  (arghelps "Disjunction to justify" "Right disjunct" "Left disjunct as term")
  (function orules=orir)
  (frag-cats rules propositional introduction backward)
  (defaults orules=orir-defaults)
  (log-p T)
  (help "Justify a disjunction by its right disjunct."))

(defun orules=orir (disj rdis par)
  (infer~compute-outline 'orir (list disj rdis) (list par)))

(defun orules=orir-defaults (disj rdis par)
  (cond ((not (com~specified-arg-p disj))
	 (list (pds~find-open-node #'logic~disjunction-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p rdis))
	 (list disj
	       (orules=check-node-on-argument disj #'logic~disjunction-p #'cadr)
	       (com~unspecified)))
	((not (com~specified-arg-p par))
	 (list disj rdis (if (and (pdsn~p disj)
				  (logic~disjunction-p (node~formula disj)))
			     (car (data~appl-arguments (node~formula disj)))
			   (com~unspecified))))
	(t (list disj rdis par))))


;;; the Disjunction Introduction Left rule:
(infer~defrule oril
	       (outline-mappings (((closed closed) oril-c)
				  ((existent existent) oril-a)
				  ((nonexistent existent) oril-f)
				  ((existent nonexistent) oril-b)))
	       (parameter-types term)
	       (help "The OR-Introduction left rule."))

(rule~defrule oril-c oril (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Checking an application of the Disjunction Introduction Left rule."))

(rule~defrule oril-a oril (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Justifying a disjunction by its left disjunct."))

(rule~defrule oril-f oril (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Deducing a disjunction from its left disjunct."))

(rule~defrule oril-b oril (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Reducing the proof of a disjunction to the proof of its left disjunct."))

(com~defcommand oril
  (argnames disjunction ldisj par)
  (argtypes ndline ndline term)
  (arghelps "Disjunction to justify" "Left disjunct" "Right disjunct as term")
  (function orules=oril)
  (frag-cats rules propositional introduction backward)
  (defaults orules=oril-defaults) 
  (log-p T)
  (help "Justify a disjunction by its left disjunct."))

(defun orules=oril (disj ldis par)
  (infer~compute-outline 'oril (list disj ldis) (list par)))

(defun orules=oril-defaults (disj ldis par)
  (cond ((not (com~specified-arg-p disj))
	 (list (pds~find-open-node #'logic~disjunction-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p ldis))
	 (list disj
	       (orules=check-node-on-argument disj #'logic~disjunction-p #'car)
	       (com~unspecified)))
	((not (com~specified-arg-p par))
	 (list disj ldis (if (and (pdsn~p disj)
				  (logic~disjunction-p (node~formula disj)))
			     (cadr (data~appl-arguments (node~formula disj)))
			   (com~unspecified))))
	(t (list disj ldis par))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;LC: Remarks to the slot 'actions':
;;Per default, the supports of the subgoals are set to the supports of the 
;;closed goal. The slot 'actions' allows the explicit manipulation of node
;;supports:
;; action ::= (which-node explicit-action*)
;; which-node ::= node | ('support' node) | NIL
;; explicit-action ::= ('sponsor' node*) | ('unsponsor' node*)
;;An action with a node N as which-node applies the specified explicit actions
;;to this node N. Where nodes following the key word 'sponsor' are added to the
;;supports of N and that following the key word 'unsponsor' are removed from the
;;supports of N.
;;The other kind of action has a labelled wich-node. Such action is applied to
;;each open node that fullfills the condition specified by the key word and the
;;succeeding nodes of the which-node. The key word 'support' imposes the open
;;node to have the given nodes in its support list. It is possible to specialize
;;this precondition by using 'sponsor' or 'unsponsor' as key words.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; the Disjunction Elimination rule:
(infer~defrule ore
	       (outline-mappings (((closed closed closed closed) ore-c)
				  ((existent existent nonexistent nonexistent)
                                   ore-b)))
	       (help "The OR elimination."))

(infer~defdummy case1
                (help "First case for case analysis."))

(infer~defdummy case2
                (help "Second case for case analysis."))

(rule~defrule ore-c ore (in base)
  (declarations 
   (meta-variables (A O) (B O) (F O)))
  (conclusion (C () F))
  (premises (P1 () (or A B))
   	    (P2 (H1) F)
            (P3 (H2) F))
  (extra-hyps (H1 () A)
              (H2 () B))
  (description "Checking an application of the Disjunction Elimination rule."))

(rule~defrule ore-b ore (in base)
  (declarations 
   (meta-variables (A O) (B O) (F O)))
  (conclusion (C () F))
  (premises (D () (or A B))
            (P1 (H1) F)
            (P2 (H2) F))
  (extra-hyps (H1 () A)
              (H2 () B))
  (sideconditions
   (pds~node-supported-by-p C D))
  (actions (P1 (sponsor H1) (unsponsor D))
	   (P2 (sponsor H2) (unsponsor D)))
  (description "Disjunction elimination"))

(com~defcommand ore
  (argnames goal disjunction)
  (argtypes ndline ndline)
  (arghelps "The goal"
	    "A disjunction")
  (function orules=ore)
  (frag-cats rules structural elimination propositional backward)
  (defaults orules=ore-defaults)
  (log-p T)
  (help "Apply rule of cases to prove goal."))

(defun orules=ore-defaults (goal disj)
  (cond ((not (com~specified-arg-p goal))
	 (list (oc~default-current-planline) (com~unspecified)))
	((not (com~specified-arg-p disj))
	 (list goal (pds~find-node-support goal #'logic~disjunction-p)))
	(t (list goal disj))))


(defun orules=ore (goal disj)
  (infer~compute-outline 'ore (list goal disj nil nil) nil))


;;; the Conjunction Introduction rule
(infer~defrule andi
	       (outline-mappings (((closed closed closed) andi-c)
				  ((existent existent existent) andi-a)
				  ((existent nonexistent nonexistent) andi-b)
				  ((nonexistent existent existent) andi-f)
				  ((existent existent nonexistent) andi-ls)
				  ((existent nonexistent existent) andi-rs)
				  ))
	       (help "The AND-Introduction."))

(rule~defrule andi-c  andi (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (description "Checking Conjunction introduction"))

(rule~defrule andi-a andi (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L1 L2))
  (description "Justifying a conjunction by its conjuncts."))

(rule~defrule andi-b andi (in base)
  (declarations 
   (meta-variables (A O) (B O))) 
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (description "Reducing the proof of a conjunction to the proof of its conjuncts."))

(rule~defrule andi-f andi (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (actions
   ((support L1 L2) (sponsor L3) (unsponsor L1 L2)))
  (description "Deducing a conjunction from two formulae."))

(rule~defrule andi-ls andi (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L1))
  (description "Given the left conjunct, reducing the proof of a conjunction to the proof of the right conjunct."))

(rule~defrule andi-rs andi (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L2))
  (description "Given the right conjunct, reducing the proof of a conjunction to the proof of the left conjunct."))

(com~defcommand andi
  (argnames conjunction lconj rconj)
  (argtypes ndline ndline ndline)
  (arghelps "Conjunction to justify" "The left conjunct" "The right conjunct")
  (function orules=andi)
  (frag-cats rules propositional introduction backward forward)
  (defaults orules=andi-defaults)
  (log-p T)
  (help "Justify a conjunction by its two conjuncts."))

(defun orules=andi-defaults (conj lconj rconj)
  (cond ((not (com~specified-arg-p conj))
	 (list (pds~find-open-node #'logic~conjunction-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p lconj))
	 (list conj
	       (orules=check-node-on-argument conj #'logic~conjunction-p #'car)
	       (com~unspecified)))
	((not (com~specified-arg-p rconj))
	 (list conj
	       lconj
	       (orules=check-node-on-argument conj #'logic~conjunction-p #'cadr)))
	(t (list conj lconj rconj))))

(defun orules=andi (conj lconj rconj)
  (infer~compute-outline 'andi (list conj lconj rconj) nil))



;;; the Conjunction Elimination Left rule:
(infer~defrule andel
	       (outline-mappings (((closed closed) andel-c)
				  ((existent existent) andel-a)
				  ((nonexistent existent) andel-f)
				  ))
	       (help "The AND Elemination Left rule."))

(rule~defrule andel-c andel (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (description "Checking an application of the Conjunction Elimination Left rule."))

(rule~defrule andel-a andel (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Application of the Conjunction Elimination Left rule."))

(rule~defrule andel-f andel (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (actions ((support P) (sponsor C)))
  (description "Deducing a left conjunct from a conjunction."))

(com~defcommand andel
  (argnames conjunction lconj)
  (argtypes ndline ndline)
  (arghelps "A Conjunction" "The left conjunct to justify")
  (function orules=andel)
  (frag-cats rules propositional elimination forward)
  (defaults orules=andel-defaults)
  (level 2)
  (log-p T)
  (help "Deducing a left conjunct from a conjunction."))

(defun orules=andel-defaults (conj lconj)
  (cond ((not (com~specified-arg-p conj))
	 (list (pds~find-support #'logic~conjunction-p) (com~unspecified)))
	((not (com~specified-arg-p lconj))
	 (list conj
	       (if (and (pdsn~p conj) (logic~conjunction-p (node~formula conj)))
		   (pds~find-open-node #'(lambda (p)
					   (data~equal p (car (data~appl-arguments (node~formula conj))))))
		 (oc~nil-argument))))
	(t (list conj lconj))))
  
(defun orules=andel (conj lconj)
  (infer~compute-outline 'andel (list lconj conj) nil))


;;; the Conjunction Elimination Right rule:
(infer~defrule ander
	       (outline-mappings (((closed closed) ander-c)
				  ((existent existent) ander-a)
				  ((nonexistent existent) ander-f)))
	       (help "The AND Elemination Right rule."))

(rule~defrule ander-c ander (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (description "Checking an application of the Conjunction Elimination Right rule."))


(rule~defrule ander-a ander (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Application of the Conjunction Elimination Right rule."))

(rule~defrule ander-f ander (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (actions ((support P) (sponsor C)))
  (description "Deducing a right conjunct from a conjunction."))

(com~defcommand ander
  (argnames conjunction rconj)
  (argtypes ndline ndline)
  (arghelps "A Conjunction" "The right conjunct to justify")
  (function orules=ander)
  (frag-cats rules propositional elimination forward)
  (defaults orules=ander-defaults)
  (level 2)
  (log-p T)
  (help "Deducing a right conjunct from a conjunction."))

(defun orules=ander-defaults (conj rconj)
  (cond ((not (com~specified-arg-p conj))
	 (list (pds~find-support #'logic~conjunction-p) (com~unspecified)))
	((not (com~specified-arg-p rconj))
	 (list conj
	       (if (and (pdsn~p conj) (logic~conjunction-p (node~formula conj)))
		   (pds~find-open-node #'(lambda (p)
					   (data~equal p (cadr (data~appl-arguments (node~formula conj))))))
		 (oc~nil-argument))))
	(t (list conj rconj))))

(defun orules=ander (conj rconj)
  (infer~compute-outline 'ander (list rconj conj) nil))


;;; the Implication Introduction rule:
(infer~defrule impi
	       (outline-mappings (((closed closed) impi-c)
;;;LC: siehe unten		  ((existent existent) impi-a)
				  ((existent nonexistent) impi-b)))
	       (help "The IMPLIES-Introduction rule."))

(rule~defrule impi-c impi (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (implies A B)))
  (premises (P (H) B))
  (extra-hyps (H () A))
  (description "Checking an application of the Implication Introduction rule."))

#|
;;;LC: Die Regel ist hier problematisch:
;; Der Regelinterpreter "uberpr"uft immer ob die Hypothesen, von denen
;; die Konklusion abhaengt (Hc) eine Obermenge der Hypothesen von denen
;; die Praemissen abhaengen (Hp). Was aber is nicht der Fall hier.
;; Der Regelinterpreter muss entsprechend angepasst werden, falls wir solche
;; Regelanwendung haben wollen (muessen).
(rule~defrule impi-a (in base)
  (arguments C P)
  (argumenttypes pdsn+node pdsn+node)
  (arghelps "An implication line" "A line with the succedent")
  (meta-variables (A O) (B O))
  (preconditions
   (P (Hp) () B)
   (C (Hc) () (implies A B)))
  (sideconditions
   (pdsn~hyp-in-but-not-in-p A Hp Hc)
   (pdsn~superset-of-without-hyp-p Hc Hp A))
  (postconditions
   (C (Hc) () (implies A B) ("ImpI" nil (P))))
  (description "Application of the Implication Introduction rule."))
|#

#|
;;;LC: Ich bin mir nicht sicher, ob es sinnvoll ist solche Richtung zu haben.
;; Die Anwendung von solcher Regel erlaubt folgendes:
;; H1 (H1)    |- A         Hyp
;; H2 (H2) ...             Hyp
;; L1 (H1 H2) |- B         ...
;; Durch Anwendung von impi-f, wird folgende Zeile hinzugefuegt:
;; L2 (H2)    |- A -> B    ImpI (L1)
;;
;; Die Implementation ist falsch und ich sehe keine suabere Moeglichkeit
;;dafuer. Es sei, man erweitert die impi-Regel um ein Parameter: die Hypothese
;;L1. Zeile L3 wird (L3 (H) () (implies A B) ("ImpI" (L1) (L2))). Das
;;entspricht aber nicht der eigentlichen ND-Regel impli. 
(rule~defrule impi-f (in base)
  (arguments L2 L1)
  (argumenttypes pdsn+node pdsn+node)
  (arghelps "An line" "A line that depends on it")
  (meta-variables (A O) (B O))
  (preconditions
   (L1 () (L1) A  ("Hyp" nil nil))
   (L2 (H) (L1) B ))
  (sideconditions (pds~support-of-p L2 L1))
  (postconditions
   (L3 (H) () (implies A B) ("ImpI" nil (L2))))
  (description "Implication introduction"))
|#

(rule~defrule impi-b impi (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (implies A B)))
  (premises (P (H) B))
  (extra-hyps (H () A))
  (actions (P (sponsor H)))
  (description "Reducing the proof of an implication to the proof of succedent using the antecedent as an additional hypothesis."))


(com~defcommand impi
  (argnames implication)
  (argtypes ndline)
  (arghelps "Implication to justify")
  (function orules=impi)
  (defaults ((orules=impi-defaults)))
  (frag-cats rules structural backward introduction propositional)
  (log-p T)
  (help "Implication Introduction"))

(defun orules=impi-defaults ()
  (pds~find-open-node #'logic~implication-p))
  
(defun orules=impi (imp)
  (infer~compute-outline 'impi (list imp nil) nil))


;;; the Implication Elimination rule:
(infer~defrule impe
	       (outline-mappings (((closed closed closed) impe-c)
				  ((existent existent existent) impe-a)
				  ((existent existent nonexistent) impe-b)
				  ((existent nonexistent existent) impe-l)
				  ((nonexistent nonexistent existent) impe-r)
				  ((nonexistent existent existent) impe-f)))
	       (help "The IMPLIES-Elimination rule."))

(rule~defrule impe-c impe (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (description "Checking an application of the Implication Elimination rule."))

(rule~defrule impe-a impe (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (sideconditions
   (pds~node-supported-by-p C P1 P2))
  (description "Application of the Implication Elimination rule."))


(rule~defrule impe-b impe (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (sideconditions
   (pds~node-supported-by-p C P1))
  (actions (P2 (unsponsor P1)))
  (description "Application of the Implication Elimination rule."))

(rule~defrule impe-f impe (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (actions ((support P1 P2) (sponsor C) (unsponsor P2)))
  (description "Forward application of the Implication Elimination rule."))

(rule~defrule impe-l impe (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (sideconditions
   (pds~node-supported-by-p C P2))
  (actions (P1 (unsponsor P2)))
  (description "Back chain on right hand side of an implication."))

(rule~defrule impe-r impe (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (actions ((support P2) (sponsor C) (unsponsor P2))
	   (P1 (unsponsor P2)))
  (description "Implication elimination"))

(com~defcommand impe
  (argnames implication antecedent succedent)
  (argtypes ndline ndline ndline)
  (arghelps "Implication" "Antecedent of the implication" "Succedent of the implication")
  (function orules=implies-e)
  (frag-cats rules propositional elimination forward backward)
  (defaults orules=impe-defaults)
  (log-p T)
  (help "Use modus ponens on two support lines."))

(defun orules=impe-defaults (impl ante succ)
  (cond ((not (com~specified-arg-p impl))
	 (list (pds~find-support #'logic~implication-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p ante))
	 (list impl
	       (if (and (pdsn~p impl) (logic~implication-p (node~formula impl)))
		   (pds~find-support #'(lambda (p)
					 (data~equal p (car (data~appl-arguments (node~formula impl))))))
		 (oc~nil-argument))
	       (com~unspecified)))
	((not (com~specified-arg-p succ))
	 (list impl ante
	       (if (and (pdsn~p impl) (logic~implication-p (node~formula impl)))
		   (pds~find-support #'(lambda (p)
					 (data~equal p (cadr (data~appl-arguments (node~formula impl))))))
		 (oc~nil-argument))))
	(t (list impl ante succ))))
  
(defun orules=implies-e (imp ante succ)
  (infer~compute-outline 'impe (list succ ante imp) nil))


;;; the Negation Introduction rule:
(infer~defrule noti
	       (outline-mappings (((closed closed) noti-c)
				  ((existent existent) noti-a)
				  ((existent nonexistent) noti-b)))
;;; LC				  ((nonexistent existent) noti-f)))
	       (help "The NOT-Introduction rule."))


(rule~defrule noti-c noti (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (description "Checking an application of the Negation Introduction rule."))

(rule~defrule noti-a noti (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (description "Application of  negation introduction"))

(rule~defrule noti-b noti (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (actions (P (sponsor H)))
  (description "Negation introduction backwards"))

#|
;;; LC: problematisch aus demselben Grund wie impi-f
(rule~defrule noti-f (in base)
  (arguments P1 P2)
  (argumenttypes pdsn+node pdsn+node)
  (arghelps "A line" "A negation line")
  (meta-variables (A O))
  (preconditions
   (P1 (H) (P1) A)
   (P2 (H) (P1) false))
  (sideconditions (pds~support-p P2 P1))
  (postconditions
   (C (H) () (not A) ("NotI" nil (P2))))
  (description "Negation introduction forwards"))
|#

(com~defcommand noti
  (argnames negation falsity)
  (argtypes ndline ndline)
  (arghelps "A negated line" "A falsity line")
  (function orules=noti)
  (frag-cats rules structural introduction propositional backward)
  (defaults orules=noti-defaults)
  (log-p T)
  (help "Introduce a negation."))

(defun orules=noti-defaults (neg fals)
  (cond ((not (com~specified-arg-p neg))
	 (list (pds~find-open-node #'logic~negation-p) (com~unspecified)))
	((not (com~specified-arg-p fals))
	 (list neg (pds~find-node-support neg #'oc~falsity-p)))
	(t (list neg fals))))

(defun orules=noti (neg fals)
  (infer~compute-outline 'noti (list neg fals) nil))

;;; negation elimination
(infer~defrule note
	       (outline-mappings (((closed closed closed) note-c)
				  ((existent existent existent) note-a)
				  ((existent existent nonexistent) note-l)
				  ((existent nonexistent existent) note-r)
				  ((nonexistent existent existent) note-f)))
	       (help "The NOT Elimination rule."))

(rule~defrule note-c note (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (description "Checking negation elimination"))

(rule~defrule note-a note (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P1 P2))
  (description "Application of negation elimination"))

(rule~defrule note-f note (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (actions
   ((support P1 P2) (sponsor C) (unsponsor P1 P2)))
  (description "Negation elimination forward"))

(rule~defrule note-r note (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P2))
  (description "Negation elimination backwards using negated line."))

(rule~defrule note-l note (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P1))
  (description "Negation elimination backward using positive line."))


(com~defcommand note
  (argnames negation line falsehood)
  (argtypes ndline ndline ndline)
  (arghelps "A negated line" "The opposite line" "Line with falsehood to prove" )
  (function orules=note)
  (frag-cats rules propositional elimination forward)
  (defaults orules=note-defaults)
  (log-p T)
  (help "Eliminate a negation."))

(defun orules=note-defaults (neg line fals)
  (cond ((not (com~specified-arg-p neg))
	 (list (pds~find-support #'logic~negation-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list neg (if (and (pdsn~p neg) (logic~negation-p (node~formula neg)))
		       (pds~find-support #'(lambda (x)
					     (data~equal x
							 (car (data~appl-arguments (node~formula neg))))))
		     (oc~nil-argument))
	       (com~unspecified)))
	((not (com~specified-arg-p fals))
	 (list neg fals (pds~find-open-node #'oc~falsity-p)))
	(t (list neg line fals))))


(defun orules=note (neg line fal)
  (infer~compute-outline 'note (list fal line neg) nil))



;;; Falsity elimination
(infer~defrule falsee
	       (outline-mappings (((closed closed) falsee-c)
				  ((existent existent) falsee-a)
				  ((existent nonexistent) falsee-b)))
	       (help "The FALSE Elimination rule."))

(rule~defrule falsee-c falsee (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P () false))
  (description "Checking (intuitionistic) falsehood elimination"))

;; was bottom-e
(rule~defrule falsee-a falsee (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P () false))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Application of (intuitionistic) falsehood elimination "))

(rule~defrule falsee-b falsee (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P () false))
  (description "(Intuitionistic) falsehood elimination backwards"))

(com~defcommand falsee
  (argnames falsehood line-to-prove)
  (argtypes ndline ndline)
  (arghelps "Falsehood line" "Line to justify")
  (function orules=falsee)
  (frag-cats rules propositional elimination backward)
  (defaults orules=falsee-defaults)
  (log-p T)
  (help "Justify a line from a falsehood line."))

(defun orules=falsee-defaults (fals line)
  (cond ((not (com~specified-arg-p fals))
	 (list (pds~find-support #'oc~falsity-p) (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list fals (oc~default-current-planline)))
	(t (list fals line))))

(defun orules=falsee (fal plan)
  (infer~compute-outline 'falsee (list plan fal) nil))


;;; existential introduction
;;; LC: wurde um Positionsliste (des Witness-terms in der Praemisse)
;; zusaetzlich zu dem Witness-term erweitert, damit moeglich
;; wird, existsi-f zu definieren und zwischen verschiedenen Anwendungen
;; zu unterscheiden:
;; Z.B. aus p(a,a) sollte man (exists x p(x,a)) als auch
;; (exists x p(x,x)) herleiten koennen.
(infer~defrule existsi
	       (outline-mappings (((closed closed) existsi-c)
				  ((existent existent) existsi-a)
				  ((nonexistent existent) existsi-f)
				  ((existent nonexistent) existsi-b)))
	       (parameter-types term position-list)
	       (help "The Existential Introduction rule."))

(rule~defrule existsi-c existsi (in base)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (orules=free-in-at-positions-p Y A PList)
   (orules=equal-replace-at-positions-p F A X PList))
  (description "Check of Existential introduction"))

(defun orules=free-in-at-positions-p (variable formula positions)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "A variable, a formula, and a position list.")
	   (effect  "None.")
	   (value   "T, iff the free occurrence positions of VARIABLE in"
		    "FORMULA corresponds to those in POSITIONS, otherwise NIL."))
  (let ((free-positions (data~substruct-positions variable formula)))
    (and (subsetp positions free-positions :test #'keim~equal)
	 (subsetp free-positions positions :test #'keim~equal))))

(defun orules=equal-replace-at-positions-p (formula1 formula2 term positions)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "Two formulas, a term, and a position list." )
	   (effect  "None.")
	   (value   "T, iff FORMULA1 equals FORMULA2 after replacing its subterms"
		    "at the given POSITIONS with TERM, otherwise NIL."))
  (term~alpha-equal formula1 (orules=replace-at-positions formula2 positions term)))

(rule~defrule existsi-a existsi (in base)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (pds~node-supported-by-p C P)
   (orules=free-in-at-positions-p Y A PList)
   (orules=equal-replace-at-positions-p F A X PList))
  (description "Application of Existential introduction"))

(rule~defrule existsi-f existsi (in base)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (orules=equal-at-positions-p X F PList))
  (computations
   (Y (orules=new-variable CC))
   (A (orules=replace-at-positions F PList Y)))
  (description "Forward application of Existential introduction"))

(defun orules=equal-at-positions-p (term formula positions)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "A term, a formula, and a position list.")
	   (effect  "None.")
	   (value   "T, iff the subterms of FORMULA at the given POSITIONS"
		    "corespond to TERM, otherwise NIL."))
  (let ((term-positions (data~substruct-positions term formula :test #'term~alpha-equal)))
    (and (subsetp term-positions positions :test #'keim~equal)
	 (subsetp positions term-positions :test #'keim~equal))))

(defun orules=new-variable (type)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "A type.")
	   (effect  "None.")
	   (value   "A variable which does not occur in the PDS environment."))
  (term~variable-create (gensym "tbr") type)) 

#|(defun orules=replace-at-positions (formula positions term)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "A formula, a position list, and a term.")
	   (effect  "None.")
	   (value   "The result of replacing in FORMULA the subterms at the given"
		    "POSITIONS with TERM."))
  (orules=replace-at-positions formula positions term)) |#

(defun orules=replace-at-positions (formula positions term)
  (do* ((current-formula formula)
	(rest-positions positions (rest rest-positions)))
      ((null rest-positions) current-formula)
    (let* ((head-position (first rest-positions)))
      (setq current-formula (data~replace-at-position current-formula head-position term
						      :replacers-downto '(data+primitive))))))
  
(rule~defrule existsi-b existsi (in base)
  (parameters (X term+term "the witness term")
	      (PList list "positions of the witness term in the premise"))
  (declarations 
   (type-variables CC)
   (meta-variables (X CC) (A O) (F O) (Y CC)))
  (conclusion (C () (exists (lam (Y CC :lookup) A))))
  (premises (P () F))
  (sideconditions
   (orules=free-in-at-positions-p Y A PList))
  (computations
   (F (orules=replace-at-positions A PList X)))
  (description "Backward application of Existential introduction"))


(com~defcommand existsi
  (argnames exists-line prem witness positionlist)
  (argtypes ndline ndline term position-list)
  (arghelps "Line to justify" "Premise" "Witness term" "List of term positions")
  (function orules=existsi)
  (frag-cats rules quantifier introduction backward)
  (defaults orules=existsi-defaults)
  (log-p T)
  (help "Justify an existential line from a particular instance."))
 
(defun orules=existsi-defaults (exists-line prem witness poslist)
  (cond ((not (com~specified-arg-p exists-line))
	 (list (oc~default-current-planline) (com~unspecified) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p prem))
	 (list exists-line (oc~nil-argument)(com~unspecified)(com~unspecified)))
	((not (com~specified-arg-p witness))
	 (list exists-line prem (com~unspecified)(com~unspecified)))
	((not (com~specified-arg-p poslist))
	 (list exists-line prem witness
	       (if (and witness prem) (data~substruct-positions witness (node~formula prem))
		 (if (logic~existential-quantification-p (node~formula exists-line))
		     (data~substruct-positions (logic~quantification-bound-variable (node~formula exists-line))
					       (logic~quantification-scope (node~formula exists-line)))
		   (com~unspecified)))))
	(t (list exists-line prem witness poslist))))

(defun orules=existsi (plan prem term position)
  (infer~compute-outline 'existsi (list plan prem) (list term position)))


;;; Existential elimination 
(infer~defrule existse
    (outline-mappings (((closed closed closed) existse-c)
                       ((existent existent nonexistent) existse-b)))
    (parameter-types termsym)
	       (help "The Existential Elimination rule."))

(rule~defrule existse-c existse (in base)
  (parameters (X term+term "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "Checking existential elimination"))

(defun orules=substitute-for-in (term variable formula)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "A term, a variable, and a formula.")
	   (effect  "None.")
	   (value   "The result of substituting TERM for each free occurrence"
		    "of VARIABLE in FORMULA."))
  (multiple-value-bind (new-formula)
      (data~replace-free-variables formula (list variable) (list term))
    (when new-formula new-formula)))

(rule~defrule existse-b existse (in base)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (A O) (B O) (X CC) (Y CC) (AX O)))
  (conclusion (C () B))
  (premises (P1 () (exists (lam (Y CC :lookup) A)))
	    (P2 (H) B))
  (extra-hyps (H () AX))
  (sideconditions
   (pds~node-supported-by-p C P1)
   (pds~not-free-in-nodes-or-hyps-p X C P1))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (actions (P2 (sponsor H) (unsponsor P1)))
  (description "Existential elimination backwards"))


#|
;;; LC: Aehnliches Problem wie impi-a
(rule~defrule existse-a (in base)
  (arguments C P1 X LH LC)
  (argumenttypes pdsn+node term+term pdsn+node pdsn+node pdsn+node)
  (arghelps  "A Line to be proved"
	     "An existential line"
	    "A term"
	    "A local hypothesis, with the instantiated  scope"
	    "The local conclusion under LH")
  (declarations
   (type-variables CC))
  (meta-variables (A (O CC)) (B O) (X CC) )
  (preconditions
   (P1 (H) () (exists A :lookup))
   ;(LH (H) (LH) (eval (beta~contract (A X))))
   (LH (H) () B)
   (LC (H) (LH) B)
   (C (H) () B))
  (sideconditions
   (and (pds~support-of-p P1 C)
	(pds~not-free-in-nodes-or-hyps-p X C P1)))
  (postconditions
   (LH (H) (LH) (eval (beta~contract (A X))) ("Hyp" (X)))
   (C (H) () B ("ExistsE" () (P1 LC))))
  (description "Application of existential elimination"))
|#


(com~defcommand existse
  (argnames exline conc parameter subgoal)
  (argtypes ndline ndline termsym ndline)
  (arghelps "An existential line" "A Line to be proved" "A term" "The second premise line")
  (function orules=existse)
  (frag-cats rules structural elimination backward quantifier)
  (defaults orules=existse-defaults)
  (log-p T)
  (help "Eliminate an existential quantifier."))

(defun orules=existse-defaults (ex-line line const prem)
  (cond ((not (com~specified-arg-p ex-line))
	   (list  (pds~find-support #'logic~existential-quantification-p)
                  (com~unspecified)
		  (com~unspecified)
		  (com~unspecified)))
	((not (com~specified-arg-p line))
	 (list ex-line
	       (oc~default-current-planline)
	       (com~unspecified)
	       (com~unspecified)))
	((not (com~specified-arg-p const))
	 (list ex-line
	       line
	       (orules=generate-default-existse ex-line)
	       (com~unspecified))) 
	((not (com~specified-arg-p prem))
	 (list ex-line
	       line
	       const
	       (oc~nil-argument))) 
	(t (list ex-line line const prem))))

(defun orules=generate-default-existse (line)
  (let* ((form (node~formula line))
	 (env (pds~environment omega*current-proof-plan))
	 (var (when (logic~existential-quantification-p form)
		(logic~quantification-bound-variable form))))
    (term~generate-term-primitive-with-new-name (keim~name var) (term~type var) 'term+constant env)))
  
(defun orules=existse (ex conc param prem2)
  (infer~compute-outline 'existse (list conc ex prem2) (list param)))



;;; universal elimination
(infer~defrule foralle
	       (outline-mappings (((closed closed) foralle-c)
				  ((existent existent) foralle-a)
				  ((nonexistent existent) foralle-f)))
	       (parameter-types term)
	       (help "The Universal Elimination rule."))

(rule~defrule foralle-c foralle (in base)
  (parameters (Y term+term "A term"))
  (declarations
   (type-variables CC)
   (meta-variables (Y CC) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () (forall (lam (X CC :lookup) A))))
  (sideconditions
   (orules=equal-replace-with-p AY A X Y))
  (description "Checking Universal elimination"))

(defun orules=equal-replace-with-p (formula1  formula2 variable term)
  (declare (edited  "20-FEB-1998")
	   (authors Lassaad)
	   (input   "Two formulas, a variable, and a term.")
	   (effect  "None.")
	   (value   "T, iff FORMULA1 equals the result of replacing each occurrence"
		    "of VARIABLE in FORMULA2 with TERM."))
  (term~alpha-equal formula1 (data~replace-free-variables formula2 (list variable) (list term))))

(rule~defrule foralle-a foralle (in base)
  (parameters (Y term+term "A term"))
  (declarations
   (type-variables CC)
   (meta-variables (Y CC) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () (forall (lam (X CC :lookup) A))))
  (sideconditions
   (pds~node-supported-by-p C P)
   (orules=equal-replace-with-p AY A X Y))
  (description "Application of Universal elimination"))


(rule~defrule foralle-f foralle (in base)
  (parameters (Y term+term "A term"))
  (declarations
   (type-variables CC)
   (meta-variables (Y CC) (AY O) (X CC) (A O)))
  (conclusion (C () AY))
  (premises (P () (forall (lam (X CC :lookup) A))))
  (computations
   (AY (orules=substitute-for-in Y X A)))
  (actions ((support P) (sponsor C)))
  (description "Universal elimination forwards"))

(com~defcommand foralle
  (argnames univ-line line term)
  (argtypes ndline ndline term)
  (arghelps "Universal line" "A line" "Term to substitute")
  (function orules=foralle)
  (frag-cats rules quantifier elimination forward)
  (defaults orules=foralle-defaults)
  (log-p T)
  (help "Instantiate a universal quantifier."))

(defun orules=foralle-defaults (univ elim term)
  (cond ((not (com~specified-arg-p univ))
	 (list (pds~find-support #'logic~universal-quantification-p) (com~unspecified) (com~unspecified)))
	((not (com~specified-arg-p elim))
	 (list univ
	       (if (and univ (logic~universal-quantification-p (node~formula univ)))
		   (let ((scope (logic~quantification-scope (node~formula univ))))
		     (pds~find-open-node #'(lambda (x) (term~alpha-match scope x))))
		 (oc~nil-argument))
	       (com~unspecified)))
	((not (com~specified-arg-p term))
	 (list univ
	       elim
	       (if (and univ
			(logic~universal-quantification-p (node~formula univ))
			elim)
		   (let* ((subst (term~alpha-match (logic~quantification-scope (node~formula univ))
						   (node~formula elim))))
		     (if subst
			 (car (subst~codomain subst))
		       (com~unspecified)))
		 (com~unspecified))))
	(t (list univ elim term))))

(defun orules=foralle (uni line term)
  (infer~compute-outline 'foralle (list line uni) (list term)))


;;; universal introduction
(infer~defrule foralli
	       (outline-mappings (((closed closed) foralli-c)
				  ((existent existent) foralli-a)
				  ((existent nonexistent) foralli-b)))
	       (parameter-types termsym)
	       (help "The Universal Elimination rule."))

(rule~defrule foralli-c foralli (in base)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C)
   (orules=equal-replace-with-p AX A Y X))
  (description "Checking universal introduction"))

(rule~defrule foralli-a foralli (in base)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C)
   (orules=equal-replace-with-p AX A Y X))
  (description "Application of universal introduction"))

(rule~defrule foralli-b foralli (in base)
  (parameters (X term+constant "the new constant"))
  (declarations
   (type-variables CC)
   (meta-variables (X CC) (Y CC) (A O) (AX O)))
  (conclusion (C () (forall (lam (Y CC :lookup) A))))
  (premises (P () AX))
  (sideconditions
   (pds~not-free-in-nodes-or-hyps-p X C))
  (computations
   (AX (orules=substitute-for-in X Y A)))
  (description "Universal introduction backwards"))


(com~defcommand foralli
  (argnames univ-line parameter line)
  (argtypes ndline termsym ndline)
  (arghelps "A Universal line to prove" "New parameter" "A line")
  (function orules=foralli)
  (frag-cats rules quantifier introduction backward)
  (defaults orules=foralli-defaults)
  (log-p T)
  (help "Introduce a universal quantifier."))

(defun orules=foralli (univ param line)
  (infer~compute-outline 'foralli (list univ line) (list param)))

(defun orules=foralli-defaults (planline param line)
    (cond ((not (com~specified-arg-p planline))
	   (list (pds~find-open-node #'logic~universal-quantification-p) (com~unspecified) (com~unspecified)))
	  ((not (com~specified-arg-p param))
	   (list planline
		 (or 
		  (barule=generate-defaults-foralli  
		   planline
		   (pds~environment omega*current-proof-plan))
		  (com~unspecified))
		 (com~unspecified)))
	  ((not (com~specified-arg-p line))
	   (list planline param (oc~nil-argument)))
	  (t (list planline param line))))

(defun barule=generate-defaults-foralli (line env)  
  (when (node~p line)
    (let ((form (node~formula line)))
      (when (logic~universal-quantification-p form)
	(let ((var (logic~quantification-bound-variable form)))
	  (term~generate-term-primitive-with-new-name 
	   (keim~name var) (term~type var) 'term+constant env)))
      )))

(infer~defrule lambda
	       (outline-mappings (((closed closed) lambda-c)
				  ((existent existent) lambda-a)))
	       (help "The rule for alpha-beta-eta equality."))

(rule~defrule lambda-c lambda (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () A))
  (sideconditions
   (lam~equal-p A B))
  (description "Check rule for alpha-beta-eta equality."))


(rule~defrule lambda-a lambda (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () A))
  (sideconditions
   (pds~node-supported-by-p C P)
   (lam~equal-p A B))
  (description "Rule for alpha-beta-eta equality."))


(com~defcommand lambda
  (argnames line1 line2)
  (argtypes ndline ndline)
  (arghelps "An open line" "Another line")
  (function orules=lambda)
  (frag-cats rules lambda structural)
  (defaults orules=lambda-defaults)
  (log-p T)
  (level 5)
  (help "Shows a line from an existing alpha-beta-eta equal one."))

(defun orules=lambda-defaults (C P)
  (cond ((not (com~specified-arg-p C))
	 (list (oc~default-current-planline) (com~unspecified)))
	((and (not (com~specified-arg-p P)))
	 (list C (if C 
		     (let ((C-formula (node~formula C)))
		       (pds~find-node-support C #'(lambda (formula)
						    (lam~equal-p formula C-formula))))
		   (com~unspecified))))
	(t (list C P))))

(defun orules=lambda (C P)
  (infer~compute-outline 'lambda (list C P) nil))


;;; the Hypothesis rule .... think about it VS.
;;(infer~defrule hyp
;;               (outline-mappings (((closed) hyp-c)
;;;;;LC: erstmal weg                ((existent) hyp-a)
;;				  ((nonexistent) hyp-f)
;;				  )))
;;               (help "The Universal Elimination rule."))
;;
;;(rule~defrule hyp-c (in base)
;;  (arguments L)
;;  (argumenttypes pdsn+node)
;;  (arghelps "A hypothesis line")
;;  (meta-variables (F O))
;;  (preconditions 
;;   (L (H) () F))
;;  (sideconditions
;;   (pdsn~hypothesis-p L))
;;  (description "Add a new hypothesis"))
;;

;; was hyp:
;;; Zwei Alternativen fuer die Implementierung von hyp-f (und aehnlichen Regelanwendungen)
;; 1. Ein zusaetzliches Parameter (siehe unten). Diese Regelanwendung fuegt die Zeile C als
;;  Hypothese ein. Man muss, noch dazu, diese Hypothese in allen Hypothesen-Listen eintragen
;;  und in den problem-assuptions usw.
;; 2. hyp-f als Taktik implementieren. Die zusaetzlichen Siteneffekte definiert man in
;;  die outline-Funktion.
;;(rule~defrule hyp-f (in base)
;;  (arguments F)
;;  (argumenttypes term+term)
;;  (arghelps "A term, to be a new hypothesis")
;;  (meta-variables (F O))
;;  (postconditions 
;;   (C () (C) F ("Hyp" (F) nil)))
;;  (description "Add a new hypothesis"))



;; extensionality rules --kk-
;; (copied from old version from sorge)

(infer~defrule exte
	       (outline-mappings (((closed closed) exte-c)
				  ((existent existent) exte-a)
                                  ((existent nonexistent) exte-b)
                                  ((nonexistent existent) exte-f)))
               (help "The Extensionality Rule (Elemination)"))

;;; The commando

(com~defcommand exte
  (argnames lowerline upperline)
  (argtypes ndline ndline)
  (arghelps "Term equation" "All-quantified sentence")
  (function orules=exte)
  (frag-cats rules elimination forward backward lambda)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Extensionality."))

(defun orules=exte (lower upper)
  (infer~compute-outline 'exte (list lower upper) nil))

(rule~defrule exte-f exte (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion (C () (= A B)))
  (premises (P () (forall (lam (X DD) (= (A X) (B X))))))
  (actions ((support P) (sponsor C)))
  (description "Eliminate extensionality of equality in a support line"))

(rule~defrule exte-b exte (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD)) (X DD)))
  (conclusion (C () (= A B)))
  (premises (P () (forall (lam (X DD :lookup) (= (A X) (B X))))))
  (computations
   (X (orules=new-variable DD)))
  (actions ((support C) (sponsor D)))
  (description "Eliminate extensionality of equality in a planned line"))

(rule~defrule exte-a exte (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion (C () (= A B)))
  (premises (P () (forall (lam (X DD) (= (A X) (B X))))))
  (sideconditions
   (pds~node-supported-by-p C P))
  (actions ((support P) (sponsor C)))
  (description "Test extensionality of equality in a planned line"))


(rule~defrule exte-c exte (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion (C () (= A B)))
  (premises (P () (forall (lam (X DD) (= (A X) (B X))))))
  (sideconditions)
  (description "Test extensionality of equality in a planned line"))


(infer~defrule exti
	       (outline-mappings (((closed closed) exti-c)
				  ((existent existent) exti-a)
                                  ((existent nonexistent) exti-b)
                                  ((nonexistent existent) exti-f)))
               (help "The Extinsionality Rule (Introduction)"))

;;; The commando

(com~defcommand exti
  (argnames lowerline upperline)
  (argtypes ndline ndline)
  (arghelps "All-quantified sentence" "Term equation")
  (function orules=exti)
  (frag-cats rules introduction forward backward lambda)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Extensionality introduction."))

(defun orules=exti (lower upper)
  (infer~compute-outline 'exti (list lower upper) nil))

(rule~defrule exti-f exti (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion (C () (forall (lam (X DD) (= (A X) (B X))))))
  (premises (P () (= A B)))
  (actions ((support P) (sponsor C)))
  (description "Introduce extensionality of equality in a support line"))


(rule~defrule exti-b exti (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion (C () (forall (lam (X DD) (= (A X) (B X))))))
  (premises (P () (= A B)))
  (actions ((support C) (sponsor P)))
  (description "Introduce extensionality of equality in a planned line"))

(rule~defrule exti-a exti (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion (C () (forall (lam (X DD) (= (A X) (B X))))))
  (premises (P () (= A B)))
  (sideconditions
   (pds~node-supported-by-p C P))
  (actions ((support C) (sponsor P)))
  (description "Test extensionality of equality in a planned line"))

(rule~defrule exti-c exti (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion (C () (forall (lam (X DD) (= (A X) (B X))))))
  (premises (P () (= A B)))
  (description "Test extensionality of equality in a planned line"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; negative functional extensionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

        -(a=b)
--------------------  Exti-neg
  -(forall x ax=bx)


|#


(infer~defrule exti-neg
	       (outline-mappings (((closed closed) exti-neg-c)
				  ((existent existent) exti-neg-a)
                                  ((existent nonexistent) exti-neg-b)
                                  ((nonexistent existent) exti-neg-f)))
               (help "The negative Extensionality Rule (Introduction)"))

;;; The commando

(com~defcommand exti-neg
  (argnames lowerline upperline)
  (argtypes ndline ndline)
  (arghelps "(negative) All-quantified sentence" "(negative) Term equation")
  (function orules=exti-neg)
  (frag-cats rules forward backward lambda introduction)
  (defaults ((oc~default-current-planline) (com~unspecified)))
  (log-p T)
  (help "Negative Extensionality introduction."))

(defun orules=exti-neg (lower upper)
  (infer~compute-outline 'exti-neg (list lower upper) nil))

(rule~defrule exti-neg-f exti-neg (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (premises
   (C () (not (= A B))))
  (conclusion
   (D () (not (forall (lam (X DD) (= (A X) (B X)))))))
  (actions ((support C) (sponsor D)))
  (sideconditions (oc=complex-type-p A))
  (description "Introduce (negative) extensionality of equality in a support line"))

(defun oc=complex-type-p (formula)
  (type~complex-p (term~type formula))) 

(rule~defrule exti-neg-b exti-neg (in base)
  (declarations 
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (conclusion
   (C () (not (forall (lam (X DD) (= (A X) (B X)))))))
  (premises
   (D () (not (= A B))))
  (actions ((support C) (sponsor D)))
  (sideconditions)
  (description "Introduce (negative) extensionality of equality in a planned line"))

(rule~defrule exti-neg-a exti-neg (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (premises
   (D () (not (= A B))))
  (conclusion
   (C () (not (forall (lam (X DD) (= (A X) (B X)))))))
  (actions ((support C) (sponsor D)))
  (sideconditions
   (pds~node-supported-by-p C D))
  (description "Test extensionality of equality in a planned line"))

(rule~defrule exti-neg-c exti-neg (in base)
  (declarations
   (type-variables DD CC)
   (meta-variables (A (CC DD)) (B (CC DD))))
  (premises
   (D () (not (= A B))))
  (conclusion
   (C () (not (forall (lam (X DD) (= (A X) (B X)))))))
  (description "Test extensionality of equality in a planned line"))




(defun orules=strip-quantors1 (term)
  (if (logic~universal-quantification-p term)
      (orules=strip-quantors1 (logic~quantification-scope term))
    term))

(defun orules=strip-quantors2 (term)
  (when (logic~universal-quantification-p term)
	(cons (logic~quantification-bound-variable term)
	      (orules=strip-quantors2 (logic~quantification-scope term)))))


;; The kappae Rule

(infer~defrule kappae
	       (outline-mappings (((nonexistent existent) kappae-f)
				  ((closed closed) kappae-c)))
	       (parameter-types type-list)
	       (help "The Kappa Elimination rule."))

(rule~defrule kappae-f kappae (in base)
  (parameters (TL cons "A type list"))
  (declarations
   (type-variables CC DD)
   (meta-variables (ATL CC) (A DD)))
  (conclusion (C () ATL))
  (premises (P () A))
  (computations
   (ATL (orules=substitute-kappa-in TL A)))
  (sideconditions
   (data~schema-p A)
   (orules=length-types-lesserequal-length-domain-p TL A))
  (actions ((support P)
	    (sponsor C)))
  (description "Kappa elimination forwards"))

(rule~defrule kappae-c kappae (in base)
  (parameters (TL cons "A type list"))
  (declarations
   (type-variables CC DD)
   (meta-variables (ATL CC) (A DD)))
  (conclusion (C () ATL))
  (premises (P () A))
  (computations
   (ATL (orules=substitute-kappa-in TL A)))
  (sideconditions
   (data~schema-p A)
   (orules=length-types-lesserequal-length-domain-p TL A))
  (actions ((support P)
	    (sponsor C)))
  (description "Kappa elimination"))

(defun orules=length-types-lesserequal-length-domain-p (type-list schema)
  (<= (length type-list)
      (length (data~schema-domain schema))))

(defun orules=substitute-kappa-in (type-list kappa-formula)
  (declare (edited  "20-FEB-1998")
	   (authors AMEIER)
	   (input   "A type-list and a kappa-formula.")
	   (effect  "None.")
	   (value   "The result of substituting the TYPE-list for the first kappa-bound variables in the"
		    "kappa-formula."))
  (let* ((schema-domain (data~schema-domain kappa-formula))
	 (first-domain-vars (subseq schema-domain 0 (length type-list)))
	 (rest-domain-vars (subseq schema-domain (length type-list)))
	 (formula (data~alpha-copy (data~schema-range kappa-formula) nil)))
    (multiple-value-bind
	(new-formula)
	(data~replace-free-variables formula first-domain-vars type-list)
      (if rest-domain-vars
	  (data~schema-create new-formula rest-domain-vars)
	new-formula))))

(com~defcommand kappae
  (argnames kappa-line line type-list)
  (argtypes ndline ndline type-list)
  (arghelps "Kappa line" "A line" "A type-list to substitute")
  (function orules=kappae)
  (frag-cats rules quantifier elimination forward)
  (defaults orules=kappae-defaults)
  (log-p T)
  (help "Instantiate a kappa quantifier."))

(defun orules=kappae (kappa-line line type)
  (infer~compute-outline 'kappae (list line kappa-line) (list type)))

(defun orules=kappae-defaults (kappa elim type)
  (cond ((not (com~specified-arg-p kappa))
	 (list (pds~find-support #'data~schema-p)
	       (com~unspecified)
	       (com~unspecified)))
	((not (com~specified-arg-p elim))
	 (list kappa
	       (if (and kappa (data~schema-p (node~formula kappa)))
		   (let ((range (data~schema-range (node~formula kappa))))
		     (pds~find-open-node #'(lambda (x)
					     (term~alpha-match range x))))
		 (oc~nil-argument))
	       (com~unspecified)))
	((not (com~specified-arg-p type))
	 (list kappa
	       elim
	       (if (and kappa
			(data~schema-p (node~formula kappa))
			elim)
		   (let* ((subst (term~alpha-match
				  (data~schema-range (node~formula kappa))
				  (node~formula elim))))
		     (if subst
			 (car (subst~codomain subst))
		       (com~unspecified)))
		 (com~unspecified))))
	(t (list kappa elim type))))


;; The kappai Rule

(infer~defrule kappai
	       (outline-mappings (((existent nonexistent) kappai-b)
				  ((closed closed) kappai-c)))
	       (parameter-types type-list)
	       (help "The Kappa Introduction rule."))


(rule~defrule kappai-b kappai (in base)
  (parameters (TL cons "A type list"))
  (declarations
   (type-variables CC DD)
   (meta-variables (ATL CC) (A DD)))
  (conclusion (C () A))
  (premises (P () ATL))
  (computations
   (ATL (orules=substitute-kappa-in TL A)))
  (sideconditions
   (data~schema-p A)
   (orules=length-types-lesserequal-length-domain-p TL A))
  (description "Kappa introduction backwards"))

(rule~defrule kappai-c kappai (in base)
  (parameters (TL cons "A type list"))
  (declarations
   (type-variables CC DD)
   (meta-variables (ATL CC) (A DD)))
  (conclusion (C () A))
  (premises (P () ATL))
  (computations
   (ATL (orules=substitute-kappa-in TL A)))
  (sideconditions
   (data~schema-p A)
   (orules=length-types-lesserequal-length-domain-p TL A))
  (description "Kappa introduction."))

(com~defcommand kappai
  (argnames kappa-line line type-list)
  (argtypes ndline ndline type-list)
  (arghelps "Open Kappa line" "A line" "A type-list to substitute")
  (function orules=kappai)
  (frag-cats rules quantifier elimination backward)
  (defaults orules=kappai-defaults)
  (log-p T)
  (help "Instantiate a kappa quantifier."))

(defun orules=kappai (kappa-line line type)
  (infer~compute-outline 'kappai (list kappa-line line) (list type)))

(defun orules=kappai-defaults (kappa elim type-list)
    (cond ((not (com~specified-arg-p kappa))
	   (list (pds~find-open-node #'data~schema-p) (com~unspecified) (com~unspecified)))
	  ((not (com~specified-arg-p elim))
	   (list kappa
		 (if (and kappa (data~schema-p (node~formula kappa)))
		     (let ((range (data~schema-range (node~formula kappa))))
		       (pds~find-support #'(lambda (x)
					     (term~alpha-match range x))))
		   (oc~nil-argument))
		 (com~unspecified)))
	  ((not (com~specified-arg-p type-list))
	   (list kappa
		 elim
		 (or (barule=generate-defaults-kappai  
		      kappa
		      (pds~environment omega*current-proof-plan))
		     (com~unspecified))))
	  (t (list kappa elim type-list))))


(defun barule=generate-defaults-kappai (line env)
  (when (node~p line)
    (let ((form (node~formula line)))
      (when (data~schema-p form)
	(let ((vars (data~schema-domain form)))
	  (mapcar #'(lambda (var)
		      (type~generate-type-primitive-with-new-name 'tc 'type+constant env))
		  vars))))))

  
;; (load "/project/omega/omega-3/theories/base/base-rules-agents.thy")


