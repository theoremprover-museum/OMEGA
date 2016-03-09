;;; -*- syntax: common-lisp; package: omega; base: 10; mode: keim -*-

(in-package :omega)

(eval-when (compile)
           (error "This file should not be compiled."))


;;;;;;;;;;;;;; Some stuff to be cleaned up ;;;;;;;;;;;;;;;;;

(defvar keim::csm*listener-priority 0)

;;;;;;;;;;;;;;;;;;;;;  Important functions for NIC-Agents ;;;;;;;;;;;;;;;;;;;;;;;




;;; is also defined test-agents.thy as agplan~contained-definitions




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;       NIC open node classifier                          ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar nic*classifier-hashtable (make-hash-table :test #'equal)
  "Hash table, indexed by rule/tactic name, that holds classifiers for the outlines of all NIC rules/tactics.")


(defun nic~classifier-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The classifier hashtable."))
  nic*classifier-hashtable)

(defun nic~set-up-nic-classifier-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None")
	   (effect  "Sets up the NIC classifier hashtable. The entries specify for each"
		    "node of the outline of a NIC rule/tactic whether this particular"
		    "node is an :e (elimination, major premise), :i (introduction), or"
		    ":n (indifferent, i.e. both) node.")
	   (value   "The  NIC classifier hashtable."))
  (setf (gethash (string-upcase "NIC-IMP-I") (nic~classifier-hashtable))        (list :I :N))
  (setf (gethash (string-upcase "NIC-AND-I") (nic~classifier-hashtable))        (list :I :N :N))
  (setf (gethash (string-upcase "NIC-FORALL-I") (nic~classifier-hashtable))     (list :I :N))
  (setf (gethash (string-upcase "NIC-NEG-I") (nic~classifier-hashtable))        (list :I :N))
  (setf (gethash (string-upcase "NIC-FALSE-C") (nic~classifier-hashtable))      (list :I :N))
  (setf (gethash (string-upcase "NIC-EXISTS-I") (nic~classifier-hashtable))     (list :I :N))
  (setf (gethash (string-upcase "NIC-EXISTS-E-i") (nic~classifier-hashtable))   (list :I :E :N))
  (setf (gethash (string-upcase "NIC-OR-I-l") (nic~classifier-hashtable))       (list :I :N))
  (setf (gethash (string-upcase "NIC-OR-I-r") (nic~classifier-hashtable))       (list :I :N))
  (setf (gethash (string-upcase "NIC-OR-E-i") (nic~classifier-hashtable))       (list :I :E :N :N))
  (setf (gethash (string-upcase "NIC-IMP-E") (nic~classifier-hashtable))        (list :N :E :N))
  (setf (gethash (string-upcase "NIC-AND-E-l") (nic~classifier-hashtable))      (list :N :E))
  (setf (gethash (string-upcase "NIC-AND-E-r") (nic~classifier-hashtable))      (list :N :E))
  (setf (gethash (string-upcase "NIC-FORALL-E") (nic~classifier-hashtable))     (list :N :E))
  (setf (gethash (string-upcase "NIC-NEG-E") (nic~classifier-hashtable))        (list :N :E :N))
  (setf (gethash (string-upcase "NIC-EXISTS-E-e") (nic~classifier-hashtable))   (list :N :E :E))
  (setf (gethash (string-upcase "NIC-OR-E-e") (nic~classifier-hashtable))       (list :N :E :E :E))
  (setf (gethash (string-upcase "NICTAC-IMP-E") (nic~classifier-hashtable))     (list :N :E :N))
  (setf (gethash (string-upcase "NICTAC-AND-E-L") (nic~classifier-hashtable))   (list :N :E))
  (setf (gethash (string-upcase "NICTAC-AND-E-R") (nic~classifier-hashtable))   (list :N :E))
  (setf (gethash (string-upcase "NICTAC-NEG-E") (nic~classifier-hashtable))     (list :N :E :N))
  (setf (gethash (string-upcase "NICTAC-OR-E-e") (nic~classifier-hashtable))    (list :N :E :E :E))
  (setf (gethash (string-upcase "NICTAC-OR-E-i") (nic~classifier-hashtable))    (list :I :E :N :N))
  (setf (gethash (string-upcase "NICTAC-OR-E") (nic~classifier-hashtable))      (list :I :E :N :N))
;;;;;;;; Additional Tactics extending NIC
  (setf (gethash (string-upcase "NICTAC-MODTOLL") (nic~classifier-hashtable))    (list :N :E :N))
  (setf (gethash (string-upcase "SOLVED-OR-RESULT-BY-LEO") (nic~classifier-hashtable))    (list :N))
  (setf (gethash (string-upcase "SOLVED-BY-LEO-PL-ATP") (nic~classifier-hashtable))    (list :N))
  (setf (gethash (string-upcase "SOLVED-BY-FO-ATP") (nic~classifier-hashtable)) (list :N))
  (setf (gethash (string-upcase "COUNTEREXAMPLE-BY-SATCHMO") (nic~classifier-hashtable)) (list :N))
  (setf (gethash (string-upcase "SET-EXT-EXPAND") (nic~classifier-hashtable))   (list :N :E))
  (setf (gethash (string-upcase "SET-EXT-CONTRACT") (nic~classifier-hashtable)) (list :I :N))
  (setf (gethash (string-upcase "DEFN-EXPAND*") (nic~classifier-hashtable))     (list :N :E))
  (setf (gethash (string-upcase "DEFNE*") (nic~classifier-hashtable))           (list :N :E))
  (setf (gethash (string-upcase "DEFN-CONTRACT*") (nic~classifier-hashtable))    (list :I :N))
  (setf (gethash (string-upcase "DEFNI*") (nic~classifier-hashtable))    (list :I :N))
  (setf (gethash (string-upcase "OTTER") (nic~classifier-hashtable))    (list :N))
  (setf (gethash (string-upcase "LEO-DERIVATION") (nic~classifier-hashtable)) (list :I :N))
  (setf (gethash (string-upcase "ANDI*") (nic~classifier-hashtable))    (list :I
									      :N))
  (setf (gethash (string-upcase "INDIRECT") (nic~classifier-hashtable))    (list
									    :I
									    :N))
  (setf (gethash (string-upcase "NOTE") (nic~classifier-hashtable))     (list :N :E :N))
  (setf (gethash (string-upcase "SIMPLIFY-WITH-CAS") (nic~classifier-hashtable)) (list :N :N))
  (omega~message "Setting up the classifier hashtable for NIC rules and tactics.")
  (nic~classifier-hashtable))

;;;; Set up hashtable, when loading this file

(nic~set-up-nic-classifier-hashtable)

(defun nic~lookup-classifier (obj)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "A string or a symbol.")
	   (effect  "None.")
	   (value   "The entry in the rule/tactic classifier hashtable associated with
obj"))
  (if (stringp obj) 
      (gethash  (string-upcase obj) (nic~classifier-hashtable))
    (when (symbolp obj) (gethash (string-upcase (symbol-name obj)) (nic~classifier-hashtable)))))

(defgeneric nic~e-i-classify-proof-context (obj)
  (declare (edited  "29-12-99")
           (authors Chris)
           (input   "A proof context")
           (effect  "None")
           (value   "The modified proof context."))
  (:method ((obj foci+pc))
	   (let ((descendants (foci~descendants (foci~active-pc))))
	     (if descendants (nic~e-i-classifier (foci~focus-line (foci~active-pc))
						 (first descendants))
	       :N)))
  (:method (obj)
	   (omega~error "~A should be a proof context." obj)))

(defun nic~e-i-classifier (focus-line father-node)
  (declare (edited  "07-JAN-2000")
	   (authors Chris)
	   (input   "A node and its father node.")
	   (effect  "None.")
	   (value   "None, the NIC e-i-classifier."))
  (let* ((father-rule-name (keim~name (pdsn~just-method father-node)))
	 (father-rule-classifier (nic~lookup-classifier father-rule-name))
	 (father-outline (cons father-node (pdsn~just-premises father-node))))
    (if (= (length father-outline) (length father-rule-classifier))
	(some #'(lambda (x y) (when (equal (keim~name focus-line)
					   (keim~name x))
				Y))
	      father-outline
	      father-rule-classifier)
      (omega~message "Inconsistency in nic-classifier-hashtable: ~A ~A" father-rule-classifier father-outline)))
  :N)
  
(defun nic~elimination-p (&optional node)
  (declare (edited  "07-JAN-2000")
	   (authors Chris)
	   (input   "Optinally a node.")
	   (effect  "None.")
	   (value   "T iff the current proof context (or optionally the node) is an"
		    "elimination context, i.e. only elimination rules are applicable."))
  (let* ((reasons (when node (pdsj~reasons (node~justification node))))
	 (node-classifier (if node
			      (if reasons
				  (nic~e-i-classifier node (pdsc~an-node (first (last
										 reasons))))
				:N)
			    (nic~e-i-classify-proof-context (foci~active-pc)))))
      (equal :E node-classifier)))
      


(defun nic~introduction-p (&optional node)
  (declare (edited  "07-JAN-2000")
	   (authors Chris)
	   (input   "Optinally a node.")
	   (effect  "None.")
	   (value   "T iff the current proof context (or optionally the node) is an"
		    "introduction context, i.e. only introduction rules are applicable."))
  (let* ((reasons (when node (pdsj~reasons (node~justification node))))
	 (node-classifier (if node
			      (if reasons
				  (nic~e-i-classifier node (pdsc~an-node (first (last
										 reasons))))
				:N)
			    (nic~e-i-classify-proof-context (foci~active-pc)))))
      (or (equal :I node-classifier)
	  (equal :N node-classifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;       NIC OR-E-INFO HASHTABLE                           ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar nic*or-e-info-hashtable (make-hash-table :test #'equal)
  "Hash table, indexed by node name, that holds nic-or-elimination information.")


(defun nic~or-e-info-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The nic-or-elimination information hashtable."))
  nic*or-e-info-hashtable)

(defvar nic*or-e-removed-info-hashtable (make-hash-table :test #'equal)
  "Hash table, indexed by node name, that holds nic-or-elimination removed information.")


(defun nic~or-e-removed-info-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The nic-or-elimination removed information hashtable."))
  nic*or-e-removed-info-hashtable)

(defun nic~lookup-or-e-info (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "The current open node in proof search")
	   (effect  "None.")
	   (value   "Looks up the entry for node in the nic-or-elimination information hashtable."))
  (gethash (string-upcase (keim~name node)) (nic~or-e-info-hashtable)))

(defun nic~lookup-or-e-removed-info (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "The current open node in proof search")
	   (effect  "None.")
	   (value   "Looks up the entry for node in the nic-or-elimination removed information hashtable."))
  (gethash (string-upcase (keim~name node)) (nic~or-e-removed-info-hashtable)))

(defun nic~remove-or-e-info (node entry)
  (declare (edited  "11-JAN-2000" "06-JAN-2000")
	   (authors Sorge Chris)
	   (input   "A node and an entry.")
	   (effect  "Removes the entry for node in the nic-or-elimination information hashtable.")
	   (value   "Undefined."))
  (let* ((node-name (string-upcase (keim~name node)))
	 (old-entry (gethash node-name (nic~or-e-info-hashtable)))
	 (new-entry (remove entry old-entry :test #'nic=or-e-info-equal)))
    (setf (gethash node-name (nic~or-e-info-hashtable)) new-entry)
    (when (term~p entry)
      (setf (gethash node-name (nic~or-e-removed-info-hashtable))
	    (cons entry (gethash node-name (nic~or-e-removed-info-hashtable)))))))

(defun nic=or-e-info-equal (x y)
  (cond ((and (term~p x) (term~p y))
	 (term~alpha-equal x y))
	((and (listp x) (listp y))
	 (and (term~alpha-equal (first x) (first y))
	      (term~alpha-equal (second x) (second y))))))

(defun nic~add-or-e-info (node disj term)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "The current open node in proof search, the disjunction term, and"
		    "the left or right disjunct")
	   (effect  "Adds an entry to the nic-or-elimination information hashtable.")
	   (value   "Undefined."))
  (when (and (logic~disjunction-p disj)
	     (or (keim~equal term (first (data~appl-arguments disj)))
		 (keim~equal term (second (data~appl-arguments disj))))
	     (not (find disj (nic~lookup-or-e-removed-info node) :test #'nic=or-e-info-equal)))
;;    (format t "Node ~A  term ~A  removed ~{~A ~}  info ~{~A ~}  test ~A~%"
;;            node
;;            disj
;;            (nic~lookup-or-e-removed-info node)
;;            (nic~lookup-or-e-info node)
;;            (not (find disj (nic~lookup-or-e-removed-info node) :test #'nic=or-e-info-equal)))
    (let* ((entries (reverse (copy-list (nic~lookup-or-e-info node))))
	   (new-entries (let ((partner (find-if #'(lambda (x)
						    (and (listp x)
							 (keim~equal (first x) disj)
							 (not (keim~equal (second x) term))))
						entries)))
			  
			  (if partner (let ((new-list (remove-if #'(lambda (x)
								     (and (listp x)
									  (keim~equal (first x) disj)))
								 entries)))
					(pushnew disj new-list :test #'nic=or-e-info-equal))
			    (pushnew (list disj term) entries :test #'nic=or-e-info-equal)))))
      (setf (gethash (string-upcase (keim~name node)) (nic~or-e-info-hashtable)) (reverse new-entries)))))

(defun nic~lookup-or-e-applicable-disjunctions (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "Looks up the disjunction candidates for or-e."))
  (let ((entries (nic~lookup-or-e-info node)))
    (mapcan #'(lambda (x) (when (and (term~p x) (logic~disjunction-p x))
			    (list x)))
	    entries)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;       NIC Introduction Rules & Corresponding Agents     ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following set of rules belong to the natural deduction calculus for intercalation
;; as presented in the PhD thesis of John Byrnes (Carnegie Mellon University, 1999).
;; In John's thesis they are presented and commented in chapter 4.1 (pages 93 ff.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rule NIC-IMP-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-IMP-I
	       (outline-mappings (((closed closed) NIC-IMP-I-c)
				  ((existent nonexistent) NIC-IMP-I-b)))
	       (help "The IMPLIES-Introduction rule."))

(rule~defrule NIC-IMP-I-c NIC-IMP-I (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (implies A B)))
  (premises (P (H) B))
  (extra-hyps (H () A))
  (description "Checking an application of the NIC implication introduction rule."))

(rule~defrule NIC-IMP-I-b NIC-IMP-I (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (implies A B)))
  (premises (P (H) B))
  (extra-hyps (H () A))
  (actions (P (sponsor H)))
  (description "Reducing the proof of an NIC implication to the proof of succedent using"
	       "the antecedent as an additional hypothesis."))

(com~defcommand NIC-IMP-I
  (argnames implication)
  (argtypes ndline)
  (arghelps "Implication to justify")
  (function nic=NIC-IMP-I)
  (frag-cats rules structural backward introduction propositional)
  (log-p T)
  (help "NIC Implication Introduction."))

(defun nic=NIC-IMP-I (imp)
  (infer~compute-outline 'NIC-IMP-I (list imp nil) nil))

(agent~defmatrix NIC-IMP-I
		 (information :c-sens)
   (agents (c-predicate (for implication)	 
                        (uses )
			(level 1)
			(information :I)	
                        (definition (when (and (nic~introduction-p (:node implication))
					       (not (agplan~repeated-line-p (:node implication))))
				      (logic~implication-p implication))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-IMP-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-IMP-E
	       (outline-mappings (((closed closed closed) NIC-IMP-E-c)
				  ((existent existent existent) NIC-IMP-E-a)
				  ))
	       (help "The NIC implication elimination rule."))

;; Remark: Only ...-a and ...-c versions are needed since for interactive and proof search
;; purposes the NIC tactic NICTAC-IMP-E should be employed.

(rule~defrule NIC-IMP-E-c NIC-IMP-E (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () A)
	    (P2 () (implies A B)))
  (description "Checking an application of the NIC implication elimination rule."))

(rule~defrule NIC-IMP-E-a NIC-IMP-E (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P1 () (implies A B))
	    (P2 () A))
  (sideconditions
   (pds~node-supported-by-p C P2 P1))
  (description "Application of the NIC implication elimination rule."))

(com~defcommand NIC-IMP-E
  (argnames  succedent implication antecedent)
  (argtypes ndline ndline ndline)
  (arghelps  "Succedent of the implication" "Implication" "Antecedent of the implication")
  (function nic=implies-e)
  (frag-cats rules propositional elimination forward backward)
  (log-p T)
  (help "Use NIC implication elimination on two support lines."))
  
(defun nic=implies-e (succ imp ante)
  (infer~compute-outline 'NIC-IMP-E (list succ ante imp) nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-IMP-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-IMP-E
		 (outline-mappings (((existent nonexistent nonexistent) NICTAC-IMP-E-b)
				    ((existent existent existent) NICTAC-IMP-E-a)
				    ((nonexistent existent existent) NICTAC-IMP-E-f)
				    ((existent existent nonexistent) NICTAC-IMP-E-r)
				    ((existent nonexistent existent) NICTAC-IMP-E-l)))
		 (expansion-function nictac=expand-NICTAC-IMP-E)
		 (parameter-types term)
		 (help "The NIC tactic for Implication elimination."))


(tac~deftactic NICTAC-IMP-E-b NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations 
                 (L3 (nictac=NICTAC-IMP-E-compute1 term))
		 (L2 (nictac=NICTAC-IMP-E-compute2 term)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond1 term L1))
   (description "Right application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute1 (term)
  (first (data~appl-arguments term)))

(defun nictac=NICTAC-IMP-E-compute2 (term)
  term)

(defun nictac=NICTAC-IMP-E-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ)))

(tac~deftactic NICTAC-IMP-E-a NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-IMP-E-sidecond2 term L1 l2 l3))
   (description "Application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-sidecond2 (term L1 L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (node~formula L1))
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-IMP-E-f NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L1 (nictac=NICTAC-IMP-E-compute3 L2)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond3 term L2 L3))
   (description "Forward Application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute3 (L2)
  (when (logic~implication-p (node~formula L2))
    (second (data~appl-arguments L2))))

(defun nictac=NICTAC-IMP-E-sidecond3 (term L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))))

(tac~deftactic NICTAC-IMP-E-l NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-IMP-E-compute4 L1 L3)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond4 term L1 L3))
   (description "Leftwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute4 (L1 L3)
  (term~appl-create (logic~implication-constant)
		    (list (node~formula L3) (node~formula L1))))

(defun nictac=NICTAC-IMP-E-sidecond4 (term L1 L3)
  (and (logic~implication-p term)
       (term~alpha-equal (first (data~appl-arguments term))
		   (node~formula L3))
       (term~alpha-equal (second (data~appl-arguments term))
		   (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-IMP-E-r NICTAC-IMP-E (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nictac=NICTAC-IMP-E-compute5 L2)))
   (sideconditions (nictac=NICTAC-IMP-E-sidecond5 term L1 L2))
   (description "Rightwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-IMP-E-compute5 (L2)
  (when (logic~implication-p (node~formula L2))
    (first (data~appl-arguments (node~formula L2)))))

(defun nictac=NICTAC-IMP-E-sidecond5 (term L1 L2)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-IMP-E (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-IMP-E outline nil)
  (tacl~end))

(com~defcommand NICTAC-IMP-E
  (argnames succedent implication antecedent term)
  (argtypes ndline ndline ndline term)
  (arghelps "Succedent" "Implication" "Antecedent" "The implication-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-IMP-E)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Implication elimination."))


(defun nictac=NICTAC-IMP-E (succedent implication antecedent term)
  (infer~compute-outline 'NICTAC-IMP-E (list succedent implication antecedent) (list term)))

(agent~defmatrix NICTAC-IMP-E
		 (information :c-sens)
 (agents
           (s-predicate (for implication)
                        (uses antecedent)
                        (exclude term)
                        (level 1)
                        (definition (pred1 implication antecedent)))
           (s-predicate (for implication)
                        (uses succedent)
                        (exclude term)
                        (level 1)
                        (definition (pred2 implication succedent)))
           (s-predicate (for implication)
                        (uses antecedent succedent)
			(exclude term)
                        (level 1)
                        (definition (and (pred1 implication antecedent)
                                         (pred2 implication succedent))))
	   (s-predicate (for implication)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal implication (:param term))))
           (c-predicate (for succedent term)
			(multiple term)
                        (uses )
                        (exclude antecedent implication)
                        (level 1)
                        (definition (when (not (agplan~repeated-line-p (:node succedent)))
				      (pred3 (:node succedent)))))
	   (c-predicate (for succedent term)
			(multiple term)
                        (uses antecedent)
                        (exclude implication)
                        (level 1)
                        (definition (pred4 (:node succedent) (:node antecedent))))
           (c-predicate (for succedent)
                        (uses implication)
                        (exclude antecedent)
                        (level 1)
                        (definition (pred2 implication succedent)))
           (s-predicate (for antecedent)
                        (uses implication)
                        (level 1)
                        (definition (pred1 implication antecedent)))
	   (function    (for term)
			(uses implication)
			(level 1)
			(definition implication))
	   (function    (for term)
			(uses antecedent succedent)
			(exclude implication)
			(level 1)
			(definition (term~appl-create (logic~implication-constant)
						      (list antecedent succedent)))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~implication-p a1)
		(term~alpha-equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
           (and (logic~implication-p a1)
                (term~alpha-equal a2 (second (data~appl-arguments a1)))))
    (pred3 (a1)
	   (let ((res (mapcan
		       #'(lambda (term)
			   (when (logic~implication-p term)
			     (let ((uni (term~alpha-unify (second (data~appl-arguments term))
							  (node~formula a1))))
			       (when uni (list (subst~apply uni term))))))
		       (agplan~str-pos-subforms (pds~node-supports a1)))))
	     (when res (list res))))
    (pred4 (a1 a2)
	   (let ((res (mapcan
		       #'(lambda (term)
			   (when (logic~implication-p term)
			     (let ((uni (term~alpha-unify term
							  (term~appl-create
							   (logic~implication-constant)
							   (list (node~formula a1)
								 (node~formula a2))))))
			       (when uni (list (subst~apply uni term))))))
		       (agplan~str-pos-subforms (pds~node-supports a1)))))
	     (when res (list res))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rule NIC-AND-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-AND-I
	       (outline-mappings (((closed closed closed) NIC-AND-I-c)
				  ((existent existent existent) NIC-AND-I-a)
				  ((existent nonexistent nonexistent) NIC-AND-I-b)
				  ((nonexistent existent existent) NIC-AND-I-f)
				  ((existent existent nonexistent) NIC-AND-I-ls)
				  ((existent nonexistent existent) NIC-AND-I-rs)
				  ))
	       (help "The AND-Introduction."))

(rule~defrule NIC-AND-I-c  NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (description "Checking NIC conjunction introduction."))

(rule~defrule NIC-AND-I-a NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L1 L2))
  (description "Justifying a NIC conjunction by its conjuncts."))

(rule~defrule NIC-AND-I-b NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O))) 
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (description "Reducing the proof of a NIC conjunction to the proof of its conjuncts."))

(rule~defrule NIC-AND-I-f NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
            (L2 () B))
  (actions
   ((support L1 L2) (sponsor L3) (unsponsor L1 L2)))
  (description "Deducing a NIC conjunction from two formulae."))

(rule~defrule NIC-AND-I-ls NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L1))
  (description "Given the left conjunct, reducing the proof of a NIC conjunction to the proof of the right conjunct."))

(rule~defrule NIC-AND-I-rs NIC-AND-I (in base)
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (L3 () (and A B)))
  (premises (L1 () A)
	    (L2 () B))
  (sideconditions
   (pds~node-supported-by-p L3 L2))
  (description "Given the right conjunct, reducing the proof of a NIC conjunction to the proof of the left conjunct."))

(com~defcommand NIC-AND-I
  (argnames conjunction lconj rconj)
  (argtypes ndline ndline ndline)
  (arghelps "Conjunction to justify" "The left conjunct" "The right conjunct")
  (function nic=NIC-AND-I)
  (frag-cats rules propositional introduction backward forward)
  (log-p T)
  (help "Justify a NIC conjunction by its two conjuncts."))

(defun nic=NIC-AND-I (conj lconj rconj)
  (infer~compute-outline 'NIC-AND-I (list conj lconj rconj) nil))

(agent~defmatrix NIC-AND-I
   (information :pl1 :c-sens)
   (agents
           (c-predicate (for Conjunction)
                        (uses )
			(exclude LConj RConj)
			(level 1)
                        (definition (when (and (nic~introduction-p (:node Conjunction))
					       (not (agplan~repeated-line-p (:node Conjunction))))
				      (logic~conjunction-p Conjunction))))
           (s-predicate (for RConj)
                        (uses Conjunction)
			(level 1)
                        (definition (pred2 Conjunction RConj))
			(help "Is Rconj the right conjunct of Conjunction."))
	   (s-predicate (for LConj)
                        (uses Conjunction)
			(level 1)
                        (definition (pred1 Conjunction LConj))
                        (help "Is Lconj the left conjunct of Conjunction.")))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~conjunction-p a1) 
                (term~alpha-equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
	   (and (logic~conjunction-p a1)
                (term~alpha-equal a2 (second (data~appl-arguments a1)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-AND-E-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-AND-E-l
	       (outline-mappings (((closed closed) NIC-AND-E-l-c)
				  ((existent existent) NIC-AND-E-l-a)
				  ((nonexistent existent) NIC-AND-E-l-f)
				  ))
	       (help "The NIC conjunction elemination left rule."))

(rule~defrule NIC-AND-E-l-c NIC-AND-E-l (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (description "Checking an application of the NIC conjunction elimination left rule."))

(rule~defrule NIC-AND-E-l-a NIC-AND-E-l (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Application of the NIC conjunction elimination left rule."))

(rule~defrule NIC-AND-E-l-f NIC-AND-E-l (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () A))
  (premises (P () (and A B)))
  (actions ((support P) (sponsor C)))
  (description "Deducing a left conjunct from a NIC conjunction."))

(com~defcommand NIC-AND-E-l
  (argnames lconj conjunction)
  (argtypes ndline ndline)
  (arghelps "The left conjunct to justify" "A Conjunction")
  (function nic=NIC-AND-E-l)
  (frag-cats rules propositional elimination forward)
  (level 2)
  (log-p T)
  (help "Deducing a left conjunct from a NIC conjunction."))

(defun nic=NIC-AND-E-l (lconj conj)
  (infer~compute-outline 'NIC-AND-E-l (list lconj conj) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-AND-E-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-AND-E-r
	       (outline-mappings (((closed closed) NIC-AND-E-r-c)
				  ((existent existent) NIC-AND-E-r-a)
				  ((nonexistent existent) NIC-AND-E-r-f)
				 ))
	       (help "The NIC conjunction elemination right rule."))

(rule~defrule NIC-AND-E-r-c NIC-AND-E-r (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (description "Checking an application of the NIC conjunction elimination right rule."))


(rule~defrule NIC-AND-E-r-a NIC-AND-E-r (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Application of the NIC conjunction elimination right rule."))

(rule~defrule NIC-AND-E-r-f NIC-AND-E-r (in base)
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () B))
  (premises (P () (and A B)))
  (actions ((support P) (sponsor C)))
  (description "Deducing a right conjunct from a NIC conjunction."))

(com~defcommand NIC-AND-E-r
  (argnames rconj conjunction)
  (argtypes ndline ndline)
  (arghelps "The right conjunct to justify" "A Conjunction")
  (function nic=NIC-AND-E-r)
  (frag-cats rules propositional elimination forward)
  (level 2)
  (log-p T)
  (help "Deducing a right conjunct from a NIC conjunction."))

(defun nic=NIC-AND-E-r (rconj conj)
  (infer~compute-outline 'NIC-AND-E-r (list rconj conj) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-AND-E-L
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-AND-E-L
		 (outline-mappings (((existent nonexistent) NICTAC-AND-E-L-b)
				    ((existent existent) NICTAC-AND-E-L-a)))
		 (expansion-function nictac=expand-NICTAC-AND-E-L)
		 (parameter-types term)
		 (help "The NIC tactic for Conjunction elimination left."))


(tac~deftactic NICTAC-AND-E-L-b NICTAC-AND-E-L (in base)
   (parameters (term term+term "The conjunction term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-AND-E-L-compute1 term)))
   (sideconditions (nictac=NICTAC-AND-E-L-sidecond1 term L1))
   (description "Right application of NIC tactic AND-E-L."))

(defun nictac=NICTAC-AND-E-L-compute1 (term)
  term)

(defun nictac=NICTAC-AND-E-L-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ)))

(tac~deftactic NICTAC-AND-E-L-a NICTAC-AND-E-L (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-AND-E-L-sidecond2 term L1 L2))
   (description "Application of NIC tactic AND-E-L."))

(defun nictac=NICTAC-AND-E-L-sidecond2 (term L1 L2)
  (and (logic~conjunction-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-AND-E-L (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-AND-E-L outline nil)
  (tacl~end))

(com~defcommand NICTAC-AND-E-L
  (argnames conc conj term)
  (argtypes ndline ndline term)
  (arghelps "Premise" "Conjunction" "The conjunction-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-AND-E-L)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Conjunction elimination left."))

(defun nictac=NICTAC-AND-E-L (conc conj term)
  (infer~compute-outline 'NICTAC-AND-E-L (list conc conj) (list term)))

(agent~defmatrix NICTAC-AND-E-L
		 (information :c-sens)
 (agents (s-predicate (for conj)
		      (uses term)
		      (level 1)
		      (definition (term~alpha-equal conj (:param term))))	   
	 (c-predicate (for conc term)
		      (multiple term)
		      (level 1)
		      (uses )
		      (definition (pred1 (:node conc)))))
 (predicates
  (pred1 (a1)
	 (let ((res (mapcan
		     #'(lambda (term)
			 (when (logic~conjunction-p term)
			   (let ((uni (term~alpha-unify (first (data~appl-arguments term))
							(node~formula a1))))
			     (when uni (list (subst~apply uni term))))))
		     (agplan~str-pos-subforms (pds~node-supports a1)))))
	   (when res (list res))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-AND-E-R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-AND-E-R
		 (outline-mappings (((existent nonexistent) NICTAC-AND-E-R-b)
				    ((existent existent) NICTAC-AND-E-R-a)))
		 (expansion-function nictac=expand-NICTAC-AND-E-R)
		 (parameter-types term)
		 (help "The NIC tactic for Conjunction elimination right."))


(tac~deftactic NICTAC-AND-E-R-b NICTAC-AND-E-R (in base)
   (parameters (term term+term "The conjunction term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-AND-E-R-compute1 term)))
   (sideconditions (nictac=NICTAC-AND-E-R-sidecond1 term L1))
   (description "Right application of NIC tactic AND-E-R."))

(defun nictac=NICTAC-AND-E-R-compute1 (term)
  term)

(defun nictac=NICTAC-AND-E-R-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ)))

(tac~deftactic NICTAC-AND-E-R-a NICTAC-AND-E-R (in base)
   (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-AND-E-R-sidecond2 term L1 L2))
   (description "Application of NIC tactic AND-E-R."))

(defun nictac=NICTAC-AND-E-R-sidecond2 (term L1 L2)
  (and (logic~conjunction-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
			 (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-AND-E-R (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-AND-E-R outline nil)
  (tacl~end))

(com~defcommand NICTAC-AND-E-R
  (argnames conc conj term)
  (argtypes ndline ndline term)
  (arghelps "Premise" "Conjunction" "The conjunction-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-AND-E-R)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Conjunction elimination right."))

(defun nictac=NICTAC-AND-E-R (conc conj term)
  (infer~compute-outline 'NICTAC-AND-E-R (list conc conj) (list term)))

(agent~defmatrix NICTAC-AND-E-R
		 (information :c-sens)
 (agents   (s-predicate (for conj)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal conj (:param term))))	   
	   (c-predicate (for conc term)
			(multiple term)
                        (uses )
			(level 1)
                        (definition (when (not (agplan~repeated-line-p (:node conc)))
				      (pred1 (:node conc))))))
 (predicates
  (pred1 (a1)
	 (let ((res (mapcan
		     #'(lambda (term)
			 (when (logic~conjunction-p term)
			   (let ((uni (term~alpha-unify (second (data~appl-arguments term))
							(node~formula a1))))
			     (when uni (list (subst~apply uni term))))))
		     (agplan~str-pos-subforms (pds~node-supports a1)))))
	   (when res (list res))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-I-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-I-l
	       (outline-mappings (((closed closed) NIC-OR-I-l-c)
				  ((existent existent) NIC-OR-I-l-a)
				  ((nonexistent existent) NIC-OR-I-l-f)
				  ((existent nonexistent) NIC-OR-I-l-b)))
	       (parameter-types term)
	       (help "The NIC disjunction left introduction rule"))

(rule~defrule NIC-OR-I-l-c NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Checking an application of the NIC disjunction introduction left rule."))

(rule~defrule NIC-OR-I-l-a NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Justifying a NIC disjunction by its left disjunct."))

(rule~defrule NIC-OR-I-l-f NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Deducing a NIC disjunction from its left disjunct."))

(rule~defrule NIC-OR-I-l-b NIC-OR-I-l (in base)
  (parameters (B term+term "the right disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () A))
  (description "Reducing the proof of a NIC disjunction to the proof of its left disjunct."))

(com~defcommand NIC-OR-I-l
  (argnames disjunction ldisj par)
  (argtypes ndline ndline term)
  (arghelps "Disjunction to justify" "Left disjunct" "Right disjunct as term")
  (function nic=NIC-OR-I-l)
  (frag-cats rules propositional introduction backward)
  (log-p T)
  (help "Justify a NIC disjunction by its left disjunct."))

(defun nic=NIC-OR-I-l (disj ldis par)
  (infer~compute-outline 'NIC-OR-I-l (list disj ldis) (list par)))

(agent~defmatrix NIC-OR-I-l
		 (information :c-sens)
   (agents (c-predicate (for disjunction par)
                        (uses )
			(exclude ldisj par)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node disjunction))
					       (not (agplan~repeated-line-p (:node disjunction)))
					       (logic~disjunction-p disjunction))
				      (list (second (data~appl-arguments disjunction))))))
           (c-predicate (for disjunction)
                        (uses ldisj)
			(exclude par)
			(level 1)
                        (definition (pred1 disjunction ldisj)))
           (c-predicate (for disjunction)
                        (uses ldisj par)
			(level 1)
                        (definition (pred2 disjunction ldisj (:param par))))
           (s-predicate (for ldisj)
                        (uses disjunction)
			(level 1)
                        (definition (pred1 disjunction ldisj))))
	   ;(function    (for par)
;                        (uses disjunction)
;			(level 1)
;                        (definition (second (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis ldis) (and (logic~disjunction-p dis)
				 (term~alpha-equal (first (data~appl-arguments dis)) ldis)))
    (pred2 (dis ldis par) (and (logic~disjunction-p ldis)
			       (term~alpha-equal (first (data~appl-arguments dis)) ldis)
			       (term~alpha-equal (second (data~appl-arguments dis)) par)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-I-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-I-r
	       (outline-mappings (((closed closed) NIC-OR-I-r-c)
				  ((existent existent) NIC-OR-I-r-a)
				  ((nonexistent existent) NIC-OR-I-r-f)
				  ((existent nonexistent) NIC-OR-I-r-b)))
	       (parameter-types term)
	       (help "The NIC disjunction introduction right rule."))


(rule~defrule NIC-OR-I-r-c NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Checking an application of the NIC disjunction introduction right rule."))

(rule~defrule NIC-OR-I-r-a NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations 
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (sideconditions
   (pds~node-supported-by-p C P))
  (description "Justifying a NIC disjunction by its right disjunct."))

(rule~defrule NIC-OR-I-r-f NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Deducing a NIC disjunction from its right disjunct."))

(rule~defrule NIC-OR-I-r-b NIC-OR-I-r (in base)
  (parameters (A term+term "the left disjunct"))
  (declarations
   (meta-variables (A O) (B O)))
  (conclusion (C () (or A B)))
  (premises (P () B))
  (description "Reducing the proof of a NIC disjunction to the proof of its right disjunct."))

(com~defcommand NIC-OR-I-r
  (argnames disjunction rdisj par)
  (argtypes ndline ndline term)
  (arghelps "Disjunction to justify" "Right disjunct" "Left disjunct as term")
  (function nic=NIC-OR-I-r)
  (frag-cats rules propositional introduction backward)
  (log-p T)
  (help "Justify a NIC disjunction by its right disjunct."))

(defun nic=NIC-OR-I-r (disj rdis par)
  (infer~compute-outline 'NIC-OR-I-r (list disj rdis) (list par)))

(agent~defmatrix NIC-OR-I-r
		 (information :c-sens)
   (agents (c-predicate (for disjunction par )
                        (uses )
			(exclude rdisj par)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node disjunction))
					       (not (agplan~repeated-line-p (:node disjunction)))
					       (logic~disjunction-p disjunction))
				      (list (first (data~appl-arguments disjunction))))))
           (c-predicate (for disjunction)
                        (uses rdisj)
			(exclude par)
			(level 1)
                        (definition (pred1 disjunction rdisj)))
           (c-predicate (for disjunction)
                        (uses rdisj par)
			(level 1)
                        (definition (pred2 disjunction rdisj (:param par))))
           (s-predicate (for rdisj)
                        (uses disjunction)
			(level 1)
                        (definition (pred1 disjunction rdisj))))
	   ;(function    (for par)
;			(level 1)
;                        (uses disjunction)
;			(level 1)
;                        (definition (first (data~appl-arguments disjunction)))))
   (predicates
    (pred1 (dis rdis) (and (logic~disjunction-p dis)
			   (term~alpha-equal (second (data~appl-arguments dis)) rdis)))
    (pred2 (dis rdis par) (and (logic~disjunction-p disjunction)
			       (term~alpha-equal (second (data~appl-arguments dis)) rdis)
			       (term~alpha-equal (first (data~appl-arguments dis)) par)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule OR-E-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To Do

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule OR-E-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To Do

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-NEG-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-NEG-E
	       (outline-mappings (((closed closed closed) NIC-NEG-E-c)
				  ((existent existent existent) NIC-NEG-E-a)
				  ((existent existent nonexistent) NIC-NEG-E-l)
				  ((existent nonexistent existent) NIC-NEG-E-r)
				  ((nonexistent existent existent) NIC-NEG-E-f)
				 ))
	       (help "The NIC negation elimination rule."))

(rule~defrule NIC-NEG-E-c NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (description "Checking NIC negation elimination rule."))

(rule~defrule NIC-NEG-E-a NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P1 P2))
  (description "Application of NIC negation elimination rule."))

(rule~defrule NIC-NEG-E-f NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
            (P2 () (not A)))
  (actions
   ((support P1 P2) (sponsor C) (unsponsor P1 P2)))
  (description "NIC negation elimination forward."))

(rule~defrule NIC-NEG-E-r NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P2))
  (description "NIC negation elimination backwards using negated line."))

(rule~defrule NIC-NEG-E-l NIC-NEG-E (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () false))
  (premises (P1 () A)
	    (P2 () (not A)))
  (sideconditions
   (pds~node-supported-by-p C P1))
  (description "NIC negation elimination backward using positive line."))


(com~defcommand NIC-NEG-E
  (argnames negation line falsehood)
  (argtypes ndline ndline ndline)
  (arghelps "A negated line" "The opposite line" "Line with falsehood to prove" )
  (function nic=NIC-NEG-E)
  (frag-cats rules propositional elimination forward)
  (log-p T)
  (help "Eliminate a NIC negation."))

(defun nic=NIC-NEG-E (neg line fal)
  (infer~compute-outline 'NIC-NEG-E (list fal line neg) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-E-e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-E-e
	       (outline-mappings (((closed closed closed closed) NIC-OR-E-e-c)
				  ((existent nonexistent nonexistent nonexistent) NIC-OR-E-e-b)))
	       (help "The OR elimination."))

(infer~defdummy case1
                (help "First case for case analysis."))

(infer~defdummy case2
                (help "Second case for case analysis."))

(rule~defrule NIC-OR-E-e-c NIC-OR-E-e (in base)
  (declarations 
   (meta-variables (A O) (B O) (F O)))
  (conclusion (C () F))
  (premises (P1 () (or A B))
   	    (P2 (H1) F)
            (P3 (H2) F))
  (extra-hyps (H1 () A)
              (H2 () B))
  (description "Checking an application of the Disjunction Elimination rule."))

;(rule~defrule NIC-OR-E-e-b NIC-OR-E-e (in base)
;  (declarations 
;   (meta-variables (A O) (B O) (F O)))
;  (conclusion (C () F))
;  (premises (D () (or A B))
;            (P1 (H1) F)
;            (P2 (H2) F))
;  (extra-hyps (H1 () A)
;              (H2 () B))
;  (sideconditions
;   (pds~node-supported-by-p C D))
;  (actions (P1 (sponsor H1) (unsponsor D))
;           (P2 (sponsor H2) (unsponsor D)))
;  (description "Disjunction elimination"))

(com~defcommand NIC-OR-E-e
  (argnames goal disjunction)
  (argtypes ndline ndline)
  (arghelps "The goal"
	    "A disjunction")
  (function nic=NIC-OR-E-e)
  (frag-cats rules structural elimination propositional backward)
  (log-p T)
  (help "Apply rule of cases to prove goal."))

(defun nic=NIC-OR-E-e (goal disj)
  (infer~compute-outline 'NIC-OR-E-e (list goal disj nil nil) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-NEG-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-NEG-E
		 (outline-mappings (((existent nonexistent nonexistent) NICTAC-NEG-E-b)
				    ((existent existent existent) NICTAC-NEG-E-a)
				    ((nonexistent existent existent) NICTAC-NEG-E-f)
				    ((existent existent nonexistent) NICTAC-NEG-E-r)
				    ((existent nonexistent existent) NICTAC-NEG-E-l)))
		 (expansion-function nictac=expand-NICTAC-NEG-E)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-NEG-E-b NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nictac=NICTAC-NEG-E-compute1 term))
		 (L2 (nictac=NICTAC-NEG-E-compute2 term)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond1 term L1))
   (description "Right application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute1 (term)
  (when (logic~negation-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-NEG-E-compute2 (term)
  term)

(defun nictac=NICTAC-NEG-E-sidecond1 (term L1)
  (and (logic~falsity-constant-p (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-NEG-E-a NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations )
   (sideconditions (nictac=NICTAC-NEG-E-sidecond2 term L1 l2 l3))
   (description "Application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-sidecond2 (term L1 L2 L3)
  (and (logic~negation-p (node~formula L2))
       (logic~falsity-constant-p (node~formula L1))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-NEG-E-f NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L1 (nictac=NICTAC-NEG-E-compute3)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond3 term L2 L3))
   (description "Forward Application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute3 ()
  (logic~falsity-constant))

(defun nictac=NICTAC-NEG-E-sidecond3 (term L2 L3)
  (and (logic~negation-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (node~formula L3))))

(tac~deftactic NICTAC-NEG-E-l NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-NEG-E-compute4 L3)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond4 term L1 L3))
   (description "Leftwards application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute4 (L3)
  (agplan~negate (node~formula L3)))

(defun nictac=NICTAC-NEG-E-sidecond4 (term L1 L3)
  (and (logic~negation-p term)
       (term~alpha-equal (first (data~appl-arguments term))
		   (node~formula L3))
       (logic~falsity-constant-p (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(tac~deftactic NICTAC-NEG-E-r NICTAC-NEG-E (in base)
   (parameters (term term+term "The negated premise term (a strictly positive subformula of
the hypotheses)."))     
   (premises L2 L3)
   (conclusions L1)
   (computations (L3 (nictac=NICTAC-NEG-E-compute5 L2)))
   (sideconditions (nictac=NICTAC-NEG-E-sidecond5 term L1 L2))
   (description "Rightwards application of NIC tactic NEG-E."))

(defun nictac=NICTAC-NEG-E-compute5 (L2)
  (when (logic~negation-p (node~formula L2))
    (first (data~appl-arguments (node~formula L2)))))

(defun nictac=NICTAC-NEG-E-sidecond5 (term L1 L2)
  (and (logic~negation-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (logic~falsity-constant-p (node~formula L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-NEG-E (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-NEG-E outline nil)
  (tacl~end))

(com~defcommand NICTAC-NEG-E
  (argnames falsity-const negated-form unnegated-form term)
  (argtypes ndline ndline ndline term)
  (arghelps "Falsity-Constant" "Negated-Formula" "Unnegated-Formula" "The negated formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-NEG-E)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Negated-Form elimination."))

(defun nictac=NICTAC-NEG-E (falsity-const negated-form unnegated-form term)
  (infer~compute-outline 'NICTAC-NEG-E (list falsity-const negated-form unnegated-form) (list term)))

(agent~defmatrix NICTAC-NEG-E
		 (information :c-sens)
 (agents
           (s-predicate (for negated-form)
                        (uses unnegated-form)
                        (exclude term)
                        (level 1)
                        (definition (pred1 negated-form unnegated-form)))
	   (s-predicate (for negated-form)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal negated-form (:param term))))
           (c-predicate (for falsity-const term)
			(multiple term)
                        (uses )
                        (exclude unnegated-form negated-form)
                        (level 1)
                        (definition (when (not (agplan~repeated-line-p (:node falsity-const)))
				      (pred3 (:node falsity-const)))))
	   (c-predicate (for falsity-const term)
			(multiple term)
                        (uses unnegated-form)
                        (exclude negated-form)
                        (level 1)
                        (definition (pred4 (:node falsity-const) (:node unnegated-form))))
           (s-predicate (for unnegated-form)
                        (uses negated-form)
                        (level 1)
                        (definition (pred1 negated-form unnegated-form)))
	   (function    (for term)
			(uses negated-form)
			(level 1)
			(definition negated-form))
	   (function    (for term)
			(uses unnegated-form)
			(exclude negated-form)
			(level 1)
			(definition (agplan~negate unnegated-form))))
   (predicates
    (pred1 (a1 a2)
	   (and (logic~negation-p a1)
		(term~alpha-equal a2 (first (data~appl-arguments a1)))))
    (pred2 (a1 a2)
           (and (logic~implication-p a1)
                (term~alpha-equal a2 (second (data~appl-arguments a1)))))
    (pred3 (a1)
	   (when (logic~falsity-constant-p (node~formula a1))
	     (let ((res (mapcan
			 #'(lambda (term)
			     (when (logic~negation-p term)
			       (list term)))
			 (agplan~str-pos-subforms (pds~node-supports a1)))))
	       (when res (list res)))))
    (pred4 (a1 a2)
	   (when (logic~falsity-constant-p (node~formula a1))
	     (let ((res (mapcan
			 #'(lambda (term)
			     (when (logic~negation-p term)
			       (let ((uni (term~alpha-unify term
							    (agplan~negate (node~formula a2)))))
				 (when uni (list (subst~apply uni term))))))
			 (agplan~str-pos-subforms (pds~node-supports a1)))))
	       (when res (list res)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Rule NIC-NEG-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-NEG-I
	       (outline-mappings (((closed closed) NIC-NEG-I-c)
				  ((existent existent) NIC-NEG-I-a)
				  ((existent nonexistent) NIC-NEG-I-b)))
	       (help "The NIC negation introduction rule."))

(rule~defrule NIC-NEG-I-c NIC-NEG-I (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (description "Checking an application of the NIC negation Introduction rule."))

(rule~defrule NIC-NEG-I-a NIC-NEG-I (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (description "Application of the NIC negation introduction rule."))

(rule~defrule NIC-NEG-I-b NIC-NEG-I (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () (not A)))
  (premises (P (H) false))
  (extra-hyps (H () A))
  (actions (P (sponsor H)))
  (description "NIC negation introduction backwards."))

(com~defcommand NIC-NEG-I
  (argnames negation falsity)
  (argtypes ndline ndline)
  (arghelps "A negated line" "A falsity line")
  (function nic=NIC-NEG-I)
  (frag-cats rules structural introduction propositional backward)
  (log-p T)
  (help "Introduce a NIC negation."))

(defun nic=NIC-NEG-I (neg fals)
  (infer~compute-outline 'NIC-NEG-I (list neg fals) nil))

(agent~defmatrix NIC-NEG-I
   (information :pl1 :c-sens)		  
   (agents
;           (s-predicate (for falsity)
;                        (uses )
;                        (exclude negation)
;                        (level 1)
;                        (definition (logic~falsity-constant-p falsity)))
;           (s-predicate (for falsity)   ;;; wie geht das hier ;;; aber das ist
;                                                ;;; doch eigentlich quatsch hier
;                        (uses negation)
;                        (level 3)
;                        (definition (pds~find-node-support (:node negation)
;                                                           #'(lambda (x) (keim~equal x falsity))))) 
	   (c-predicate (for negation)
                        (uses )
			(exclude negation)
			(level 1)
                        (definition (when (and (nic~introduction-p (:node negation))
					       (not (agplan~repeated-line-p (:node negation))))
				      (logic~negation-p negation))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-FALSE-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-FALSE-C
	       (outline-mappings (((closed closed) NIC-FALSE-C-c)
				  ((existent existent) NIC-FALSE-C-a)
				  ((existent nonexistent) NIC-FALSE-C-b)))
	       (help "The NOT-Introduction rule."))

(rule~defrule NIC-FALSE-C-c NIC-FALSE-C (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P (H) false))
  (extra-hyps (H () (not A)))
  (description "Checking an application of the NIC falsity introduction rule."))

(rule~defrule NIC-FALSE-C-a NIC-FALSE-C (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P (H) false))
  (extra-hyps (H () (not A)))
  (description "Application of the NIC falsity introduction rule."))

(rule~defrule NIC-FALSE-C-b NIC-FALSE-C (in base)
  (declarations
   (meta-variables (A O)))
  (conclusion (C () A))
  (premises (P (H) false))
  (extra-hyps (H () (not A)))
  (actions (P (sponsor H)))
  (description "NIC falsity introduction backwards."))

(com~defcommand NIC-FALSE-C
  (argnames conc falsity)
  (argtypes ndline ndline)
  (arghelps "A unnegated line" "A falsity line")
  (function nic=NIC-FALSE-C)
  (frag-cats rules structural false propositional backward)
  (log-p T)
  (help "Introduce a NIC falsity."))

(defun nic=NIC-FALSE-C (form fals)
  (infer~compute-outline 'NIC-FALSE-C (list form fals) nil))

(agent~defmatrix NIC-FALSE-C
   (information :pl1 :c-sens)		  
   (agents (c-predicate (for conc)
                        (uses )
			(exclude falsity)
			(level 1)
			(help "See restriction in John's Diss mentioned on page 130.")
                        (definition (when (and (nic~introduction-p (:node conc))
					       (not (agplan~repeated-line-p conc)))
				      (and (not (logic~falsity-constant-p conc))
					   (or (logic~atom-p conc)
					       (logic~disjunction-p conc)
					       (logic~existential-quantification-p conc))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;        Special NIC Weaken (=Subst) tactic               ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To do: Modify weaken in directed equality substitution


(infer~deftactic NICTAC-WEAKEN
	       (outline-mappings (((existent existent) NICTAC-WEAKEN-a)))
	       (expansion-function nictac=expand-NICTAC-WEAKEN)
	       (help "The NIC WEAKENing tactic."))

(tac~deftactic NICTAC-WEAKEN-a NICTAC-WEAKEN (in base)
   (premises P)
   (conclusions C)
   (sideconditions (nictac=NICTAC-WEAKEN-a-sidecond P C))
   (description "Application of the NIC Waekening tactic."))

(defun nictac=NICTAC-WEAKEN-a-sidecond (P C)
  (and 
   (term~alpha-equal (node~formula P) (node~formula C))
   (pds~node-supported-by-p C P)))

(com~defcommand NICTAC-WEAKEN
  (argnames lowerline upperline)
  (argtypes ndline ndline)
  (arghelps "Line to justify" "Already-derived line")
  (function nictac=NICTAC-WEAKEN)
  (frag-cats rules structural elimination introduction)
  (log-p T)
  (help "Justify a line from an unifiable earlier-derived line."))

(defun nictac=NICTAC-WEAKEN (lower upper)
  (infer~compute-outline 'NICTAC-WEAKEN (list lower upper) nil))


(agent~defmatrix NICTAC-WEAKEN
		 (information :c-sens)
   (agents (c-predicate (for lowerline upperline)
                        (uses )
			(level 1)
                        (definition (when (not (agplan~repeated-line-p (:node lowerline)))
				      (pred1 (:node lowerline))))))
   (predicates
    (pred1 (a1)
	   (some #'(lambda (x)
		     (when (term~alpha-equal (node~formula x) (node~formula a1))
		       (list x)))
		 (pds~node-supports a1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rule NIC-OR-E-i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~defrule NIC-OR-E-i
	       (outline-mappings (((closed closed closed closed) NIC-OR-E-i-c)
				  ((existent existent nonexistent nonexistent)
                                   NIC-OR-E-i-b)))
	       (help "The NIC disjunction elimination rule."))

(rule~defrule NIC-OR-E-i-c NIC-OR-E-i (in base)
  (declarations 
   (meta-variables (A O) (B O) (F O)))
  (conclusion (C () F))
  (premises (P1 () (or A B))
   	    (P2 (H1) F)
            (P3 (H2) F))
  (extra-hyps (H1 () A)
              (H2 () B))
  (description "Checking an application of the NIC disjunction elimination rule."))

(rule~defrule NIC-OR-E-i-b NIC-OR-E-i (in base)
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
  (description "NIC disjunction elimination."))

(com~defcommand NIC-OR-E-i
  (argnames goal disjunction)
  (argtypes ndline ndline)
  (arghelps "The goal"
	    "A disjunction")
  (function nic=NIC-OR-E-i)
  (frag-cats rules structural elimination propositional backward)
  (log-p T)
  (help "Apply NIC disjunction elimination rule."))

(defun nic=NIC-OR-E-i (goal disj)
  (infer~compute-outline 'NIC-OR-E-i (list goal disj nil nil) nil))


#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-OR-E-e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-OR-E-e
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) NICTAC-OR-E-e-b)
				    ((existent existent nonexistent nonexistent) NICTAC-OR-E-e-r)))
		 (expansion-function nictac=expand-NICTAC-OR-E-e)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-OR-E-e-b NICTAC-OR-E-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-OR-E-e-compute1 term))
		 (H1 (nictac=NICTAC-OR-E-e-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-e-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-e-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-e-compute4 L1)))   
   (sideconditions (nictac=NICTAC-OR-E-e-sidecond1 term L1))
   (description "Backward application of NIC tactic OR-E-e."))

(defun nictac=NICTAC-OR-E-e-compute1 (term)
  term)

(defun nictac=NICTAC-OR-E-e-compute2 (term)
  (when (logic~disjunction-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-e-compute3 (term)
  (when (logic~disjunction-p term)
    (second (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-e-compute4 (L1)
  (node~formula L1))

(defun nictac=NICTAC-OR-E-e-sidecond1 (term L1)
  (and (logic~disjunction-p term)
       (agplan~str-pos-subf-p L1 (pds~node-supports L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))


(tac~deftactic NICTAC-OR-E-e-r NICTAC-OR-E-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (H1 (nictac=NICTAC-OR-E-e-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-e-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-e-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-e-compute4 L1)))
   (sideconditions (nictac=NICTAC-OR-E-e-sidecond2 term L1 L2))
   (description "Backward application of NIC tactic OR-E-e."))

(defun nictac=NICTAC-OR-E-e-sidecond2 (term L1 L2)
  (and (logic~disjunction-p term)
       (term~alpha-equal term (node~formula L2))
       (agplan~str-pos-subf-p L1 (pds~node-supports L1))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-OR-E-e (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-OR-E-e outline nil)
  (tacl~end))

(com~defcommand NICTAC-OR-E-e
  (argnames conc disjunction new-conc1 new-conc2 term)
  (argtypes ndline ndline ndline ndline term)
  (arghelps "Conclusion" "Disjunctionula" "New-Conclusion 1" "New-Conclusion 2" "The disjunction formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-OR-E-e)
  (frag-cats tactics base)
  (log-p T)
  (help "NIC tactic Disjunction elimination."))

(defun nictac=NICTAC-OR-E-e (conc disjunction new-conc1 new-conc2 term)
  (infer~compute-outline 'NICTAC-OR-E-e (list conc disjunction new-conc1 new-conc2) (list term)))

(agent~defmatrix NICTAC-OR-E-e
		 (information :c-sens)
 (agents
           (s-predicate (for disjunction)
                        (uses conc)
                        (exclude term)
                        (level 1)
                        (definition (pred2 conc disjunction)))
	   (s-predicate (for disjunction)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal disjunction (:param term))))
           (c-predicate (for conc term)
			(multiple term)
                        (uses )
                        (exclude new-conc1 new-conc2 disjunction)
                        (level 1)
                        (definition (when (not (agplan~repeated-line-p (:node conc)))
				      (pred1 (:node conc)))))
	   (function    (for term)
			(uses disjunction)
			(level 1)
			(definition disjunction)))
   (predicates
    (pred2 (conc disjunction)
	   (when conc ;;; dummy, declare ignore does not work
	     (logic~disjunction-p disjunction)))
    (pred1 (a1)
	   (when (agplan~str-pos-subf-p (node~formula a1) (pds~node-supports a1))
	     (let ((res (mapcan
			 #'(lambda (term)
			     (when (logic~disjunction-p term)
			       (list term)))
			 (agplan~str-pos-subforms (pds~node-supports a1)))))
	       (when res (list res)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-OR-E-i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-OR-E-i
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) NICTAC-OR-E-i-b)
				    ((existent existent nonexistent nonexistent) NICTAC-OR-E-i-r)))
		 (expansion-function nictac=expand-NICTAC-OR-E-i)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-OR-E-i-b NICTAC-OR-E-i (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-OR-E-i-compute1 term))
		 (H1 (nictac=NICTAC-OR-E-i-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-i-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-i-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-i-compute4 L1)))   
   (sideconditions (nictac=NICTAC-OR-E-i-sidecond1 term L1))
   (description "Backward application of NIC tactic OR-E-i."))

(defun nictac=NICTAC-OR-E-i-compute1 (term)
  term)

(defun nictac=NICTAC-OR-E-i-compute2 (term)
  (when (logic~disjunction-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-i-compute3 (term)
  (when (logic~disjunction-p term)
    (second (data~appl-arguments term))))

(defun nictac=NICTAC-OR-E-i-compute4 (L1)
  (node~formula L1))

(defun nictac=NICTAC-OR-E-i-sidecond1 (term L1)
  (and (logic~disjunction-p term)
       (agplan~str-pos-subf-p term (pds~node-supports L1))))


(tac~deftactic NICTAC-OR-E-i-r NICTAC-OR-E-i (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (H1 (nictac=NICTAC-OR-E-i-compute2 term))
		 (H2 (nictac=NICTAC-OR-E-i-compute3 term))
		 (L3 (nictac=NICTAC-OR-E-i-compute4 L1))
		 (L4 (nictac=NICTAC-OR-E-i-compute4 L1)))
   (sideconditions (nictac=NICTAC-OR-E-i-sidecond2 term L1 L2))
   (description "Backward application of NIC tactic OR-E-i."))

(defun nictac=NICTAC-OR-E-i-sidecond2 (term L1 L2)
  (and (logic~disjunction-p term)
       (term~alpha-equal term (node~formula L2))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-OR-E-i (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-OR-E-i outline nil)
  (tacl~end))

(com~defcommand NICTAC-OR-E-i
  (argnames conc disjunction new-conc1 new-conc2 term)
  (argtypes ndline ndline ndline ndline term)
  (arghelps "Conclusion" "Disjunction" "New-Conclusion 1" "New-Conclusion 2" "The disjunction formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-OR-E-i)
  (frag-cats tactics base)
  (log-p T)
  (help "NIC tactic Disjunction elimination."))

(defun nictac=NICTAC-OR-E-i (conc disjunction new-conc1 new-conc2 term)
  (infer~compute-outline 'NICTAC-OR-E-i (list conc disjunction new-conc1 new-conc2) (list term)))

(agent~defmatrix NICTAC-OR-E-i
		 (information :c-sens)
 (agents
           (s-predicate (for disjunction)
                        (uses conc)
                        (exclude term)
                        (level 1)
                        (definition (logic~disjunction-p disjunction)))
	   (s-predicate (for disjunction)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal disjunction (:param term))))
           (c-predicate (for conc term)
			(multiple term)
                        (uses )
                        (exclude new-conc1 new-conc2 disjunction)
                        (level 1)
                        (definition  (when (not (agplan~repeated-line-p (:node conc)))
				       (pred1 (:node conc)))))
	   (function    (for term)
			(uses disjunction)
			(level 1)
			(definition disjunction)))
   (predicates
    (pred1 (a1)
	   (let ((res (mapcan
		       #'(lambda (term)
			   (when (logic~disjunction-p term)
			     (list term)))
		       (agplan~str-pos-subforms (pds~node-supports a1)))))
	     (when res (list res))))))

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICDUMMY-OR-E-l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand NICDUMMY-OR-E-l
  (argnames conc term)
  (argtypes ndline term)
  (arghelps "Conclusion" "The disjunction formula")
  (function nictac=NICDUMMY-OR-E-l)
  (frag-cats tactics base dummy)
  (log-p T)
  (help "Special dummy NIC tactic Disjunction elimination left."))


(defun nictac=NICDUMMY-OR-E-l (conc term)
  (when (logic~disjunction-p term)
    (let ((left-disj (first (data~appl-arguments term))))
      (auto=message "~A: Adding ~A to nodes ~{~A ~}." "NICDUMMY-OR-E-l" (list term left-disj)
		    (cons conc (foci~descendants (foci~active-pc))))
      (mapcar #'(lambda (x) (nic~add-or-e-info x term left-disj))
	      (cons conc (foci~descendants (foci~active-pc)))))
    (sugg~reset)))
    ;;    (proc~without-scheduling
;;     (sugg~reset)
;;     (setf (bb~reset-state (bb~find-blackboard 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~get-surveyor 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~find-parameter-agent 'nic-false-c '(conc) nil
;;                                                          '(falsity)))
;;           sugg*reset))))

(defun nictac~reduce-or-e-l-termsuggestions (termlist hash-entries)
  (let ((reslist))
    (dolist (disj termlist reslist)
      (when (not (find-if #'(lambda (x) (or (and (term~p x) (term~alpha-equal x disj))
					    (and (listp x)
						 (term~alpha-equal (first x) disj)
						 (term~alpha-equal (first (data~appl-arguments (first x)))
							     (second x)))))
		       ;;; dummy-or-e-l has already been executed
			  hash-entries))
	(push disj reslist)))))

(agent~defmatrix NICDUMMY-OR-E-l
		  (information :c-sens)
    (agents (c-predicate (for conc term)
			 (multiple term)
			 (level 1)
			 (definition  (when (not (agplan~repeated-line-p (:node conc)))
					(pred1 (:node conc))))))
    (predicates
     (pred1 (a1)
	    (let ((res (mapcan
			#'(lambda (term)
			    (when (and (logic~disjunction-p term)
				       (term~alpha-equal (first (data~appl-arguments term))
						   (node~formula a1)))
			      (list term)))
			(agplan~str-pos-subforms (pds~node-supports a1))))
		  (given-entries (nic~lookup-or-e-info a1)))
	      (when res
		(if given-entries
		    (let ((new-res (nictac~reduce-or-e-l-termsuggestions res given-entries)))
		      (when new-res (list new-res)))
		  (list res)))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICDUMMY-OR-E-r
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand NICDUMMY-OR-E-r
  (argnames conc term)
  (argtypes ndline term)
  (arghelps "Conclusion" "The disjunction formula")
  (function nictac=NICDUMMY-OR-E-r)
  (frag-cats tactics base dummy)
  (log-p T)
  (help "Special dummy NIC tactic Disjunction elimination right."))

(defun nictac=NICDUMMY-OR-E-r (conc term)
  (when (logic~disjunction-p term)
    (let ((right-disj (second (data~appl-arguments term))))
      (auto=message "~A: Adding ~A to nodes ~{~A ~}." "NICDUMMY-OR-E-r" (list term right-disj)
		   (cons conc (foci~descendants (foci~active-pc))))
      (mapcar #'(lambda (x) (nic~add-or-e-info x term right-disj))
	      (cons conc (foci~descendants (foci~active-pc)))))
    (sugg~reset)))
;;
;;    (proc~without-scheduling
;;     (sugg~reset)
;;     (setf (bb~reset-state (bb~find-blackboard 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~get-surveyor 'nic-false-c)) sugg*reset)
;;     (setf (agent~reset-state (agent~find-parameter-agent 'nic-false-c '(conc) nil
;;							  '(falsity))) sugg*reset))))

(defun nictac~reduce-or-e-r-termsuggestions (termlist hash-entries)
  (let ((reslist))
    (dolist (disj termlist reslist)
      (when (not (find-if #'(lambda (x) (or (and (term~p x) (term~alpha-equal x disj))
					    (and (listp x)
						 (term~alpha-equal (first x) disj)
						 (term~alpha-equal (second (data~appl-arguments (first x)))
							     (second x)))))
		       ;;; dummy-or-e-r has already been executed
			  hash-entries))
	(push disj reslist)))))

(agent~defmatrix NICDUMMY-OR-E-r
		 (information :c-sens)
    (agents (c-predicate (for conc term)
			 (multiple term)
			 (level 1)
			 (definition  (when (not (agplan~repeated-line-p (:node conc)))
					(pred1 (:node conc))))))
    (predicates
     (pred1 (a1)
	    (let ((res (mapcan
			#'(lambda (term)
			    (when (and (logic~disjunction-p term)
				       (term~alpha-equal (second (data~appl-arguments term))
						   (node~formula a1)))
			      (list term)))
			(agplan~str-pos-subforms (pds~node-supports a1))))
		  (given-entries (nic~lookup-or-e-info a1)))
	      (when res
		(if given-entries
		    (let ((new-res (nictac~reduce-or-e-r-termsuggestions res given-entries)))
		      (when new-res (list new-res)))
		  (list res)))))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-OR-E
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(infer~deftactic NICTAC-or-e
		 (outline-mappings (((existent nonexistent nonexistent nonexistent) NICTAC-or-e-b)
				    ((existent existent nonexistent nonexistent) NICTAC-or-e-r)))
		 (expansion-function nictac=expand-NICTAC-or-e)
		 (parameter-types term)
		 (help "The NIC tactic for Negation elimination."))


(tac~deftactic NICTAC-or-e-b NICTAC-or-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (L2 (nictac=NICTAC-or-e-compute1 term))
		 (H1 (nictac=NICTAC-or-e-compute2 term))
		 (H2 (nictac=NICTAC-or-e-compute3 term))
		 (L3 (nictac=NICTAC-or-e-compute4 L1))
		 (L4 (nictac=NICTAC-or-e-compute4 L1)))   
   (sideconditions (nictac=NICTAC-or-e-sidecond1 term L1))
   (description "Backward application of NIC tactic or-e."))

(defun nictac=NICTAC-or-e-compute1 (term)
  term)

(defun nictac=NICTAC-or-e-compute2 (term)
  (when (logic~disjunction-p term)
    (first (data~appl-arguments term))))

(defun nictac=NICTAC-or-e-compute3 (term)
  (when (logic~disjunction-p term)
    (second (data~appl-arguments term))))

(defun nictac=NICTAC-or-e-compute4 (L1)
  (node~formula L1))

(defun nictac=NICTAC-or-e-sidecond1 (term L1)
  (and (logic~disjunction-p term)
       (agplan~str-pos-subf-p term (pds~node-supports L1))))


(tac~deftactic NICTAC-or-e-r NICTAC-or-e (in base)
   (parameters (term term+term "The disjunction premise term (a strictly positive subformula of
the hypotheses)."))
   (hypotheses ((H1 L3) "H1 is the hypotheses for L3")
	       ((H2 L4) "H2 is the hypotheses for L4"))
   (premises L2 L3 L4)
   (conclusions L1)
   (computations (H1 (nictac=NICTAC-or-e-compute2 term))
		 (H2 (nictac=NICTAC-or-e-compute3 term))
		 (L3 (nictac=NICTAC-or-e-compute4 L1))
		 (L4 (nictac=NICTAC-or-e-compute4 L1)))
   (sideconditions (nictac=NICTAC-or-e-sidecond2 term L1 L2))
   (description "Backward application of NIC tactic or-e."))

(defun nictac=NICTAC-or-e-sidecond2 (term L1 L2)
  (and (logic~disjunction-p term)
       (term~alpha-equal term (node~formula L2))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-or-e (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-or-e outline nil)
  (tacl~end))

(com~defcommand NICTAC-or-e
  (argnames conc disjunction new-conc1 new-conc2 term)
  (argtypes ndline ndline ndline ndline term)
  (arghelps "Conclusion" "Disjunction" "New-Conclusion 1" "New-Conclusion 2" "The disjunction formula (strictly
positiv-subformula of hypotheses)")
  (function nictac=NICTAC-or-e)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Disjunction elimination."))

(defun nictac=NICTAC-or-e (conc disjunction new-conc1 new-conc2 term)
  (auto=message "~% Apply NICTAC-or-e to node ~A with disj ~A" conc term)
  (infer~compute-outline 'NICTAC-or-e (list conc disjunction new-conc1 new-conc2) (list term))
  (nic~remove-or-e-info conc term))

(agent~defmatrix NICTAC-or-e
		 (information :c-sens) 
 (agents
           (s-predicate (for disjunction)
                        (uses conc)
                        (exclude term)
                        (level 1)
                        (definition (logic~disjunction-p disjunction)))
	   (s-predicate (for disjunction)
                        (uses term)
                        (level 1)
                        (definition (term~alpha-equal disjunction (:param term))))
           (c-predicate (for conc term)
			(multiple term)
                        (uses )
                        (exclude new-conc1 new-conc2 disjunction)
                        (level 1)
                        (definition  (when (not (agplan~repeated-line-p (:node conc)))
				       (pred1 (:node conc)))))
	   (function    (for term)
			(uses disjunction)
			(level 1)
			(definition disjunction)))
   (predicates
    (pred1 (a1)
	   (let ((res (remove-if-not #'(lambda (x)
					 (agplan~str-pos-subf-p x (pds~node-supports a1)))
				     (nic~lookup-or-e-applicable-disjunctions a1))))
	     (when res (list res))))))

(pushnew :nic-pl *features*)



(defun nictac=logic-negate (formula)
  (if (logic~negation-p formula)
      (car (data~appl-arguments formula))
    (logic~negate formula)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tactic NICTAC-MODTOLL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(infer~deftactic NICTAC-MODTOLL
		 (outline-mappings (((existent nonexistent nonexistent) NICTAC-MODTOLL-b)
				    ((existent existent existent) NICTAC-MODTOLL-a)
				    ((nonexistent existent existent) NICTAC-MODTOLL-f)
				    ((existent existent nonexistent) NICTAC-MODTOLL-r)
				    ((existent nonexistent existent) NICTAC-MODTOLL-l)))
		 (expansion-function nictac=expand-NICTAC-MODTOLL)
		 (parameter-types term)
		 (help "The NIC tactic for Implication elimination."))

;; backward application of Modus Tollens

(tac~deftactic NICTAC-MODTOLL-b NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations 
		(L3 (nictac=NICTAC-MODTOLL-compute1 term))
		(L2 (nictac=NICTAC-MODTOLL-compute2 term)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond1 term L1))
	       (description "Right application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute1 (term)
  (nictac=logic-negate (second (data~appl-arguments term))))

(defun nictac=NICTAC-MODTOLL-compute2 (term)
  term)

(defun nictac=NICTAC-MODTOLL-sidecond1 (term succ)
  (agplan~str-pos-subf-p term (pds~node-supports succ))) ;; test if neg(L1) = first (term) ?


;; closing Modus Tollens

(tac~deftactic NICTAC-MODTOLL-a NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations )
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond2 term L1 l2 l3))
	       (description "Application of NIC tactic IMP-E."))


(defun nictac=NICTAC-MODTOLL-sidecond2 (term L1 L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L1)))
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L3)))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

;; Applying Modus Tollens in forward direction

(tac~deftactic NICTAC-MODTOLL-f NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L1 (nictac=NICTAC-MODTOLL-compute3 L2)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond3 term L2 L3))
	       (description "Forward Application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute3 (L2)
  (when (logic~implication-p (node~formula L2))
    (nictac=logic-negate (first (data~appl-arguments L2)))))

(defun nictac=NICTAC-MODTOLL-sidecond3 (term L2 L3)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (second (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L3)))))

;; Applying Modus Tollen backwards in left direction


(tac~deftactic NICTAC-MODTOLL-l NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L2 (nictac=NICTAC-MODTOLL-compute4 L1 L3)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond4 term L1 L3))
	       (description "Leftwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute4 (L1 L3)
  (term~appl-create (logic~implication-constant)
		    (list (nictac=logic-negate (node~formula L1))
						  (nictac=logic-negate (node~formula L3)))))

(defun nictac=NICTAC-MODTOLL-sidecond4 (term L1 L3)
  (and (logic~implication-p term)
       (term~alpha-equal (second (data~appl-arguments term))
		   (nictac=logic-negate (node~formula L3)))
       (term~alpha-equal (first (data~appl-arguments term))
		   (nictac=logic-negate (node~formula L1)))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

;; Backwards and rightwards, computing the negated succedent

(tac~deftactic NICTAC-MODTOLL-r NICTAC-MODTOLL (in base)
	       (parameters (term term+term "The implication term (a strictly positive subformula of
the hypotheses)."))     
	       (premises L2 L3)
	       (conclusions L1)
	       (computations (L3 (nictac=NICTAC-MODTOLL-compute5 L2)))
	       (sideconditions (nictac=NICTAC-MODTOLL-sidecond5 term L1 L2))
	       (description "Rightwards application of NIC tactic IMP-E."))

(defun nictac=NICTAC-MODTOLL-compute5 (L2)
  (when (logic~implication-p (node~formula L2))
    (nictac=logic-negate (second (data~appl-arguments (node~formula L2))))))

(defun nictac=NICTAC-MODTOLL-sidecond5 (term L1 L2)
  (and (logic~implication-p (node~formula L2))
       (term~alpha-equal (node~formula L2) term)
       (term~alpha-equal (first (data~appl-arguments (node~formula L2)))
		   (nictac=logic-negate (node~formula L1)))
       (agplan~str-pos-subf-p term (pds~node-supports L1))))

(defun nictac=expand-NICTAC-MODTOLL (outline parameters)
  (tacl~init outline)
  (tacl~apply 'NIC-IMP-E outline nil)
  (tacl~end))

(com~defcommand NICTAC-MODTOLL
  (argnames negantecedent implication negsucc term)
  (argtypes ndline ndline ndline term)
  (arghelps "Negated antecedent" "Implication" "Negated succedent" "The implication-term (strictly positiv-subformula of hypotheses)")
  (function nictac=NICTAC-MODTOLL)
  (frag-cats tactics base elimination)
  (log-p T)
  (help "NIC tactic Modus tollens."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Agent fuer IMP-E / Modus Tollens  ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nictac=NICTAC-MODTOLL (succedent implication antecedent term)
  (infer~compute-outline 'NICTAC-MODTOLL (list succedent implication antecedent) (list term)))

(agent~defmatrix NICTAC-MODTOLL
 		 (information :c-sens)
		 (agents
		  (s-predicate (for implication)
			       (uses negsucc)
			       (exclude term)
			       (level 1)
			       (definition (pred1 implication negsucc)))
		  (s-predicate (for implication)
			       (uses negantecedent)
			       (exclude term)
			       (level 1)
			       (definition (pred2 implication negantecedent)))
		  (s-predicate (for implication)
			       (uses negsucc negantecedent)
			       (exclude term)
			       (level 1)
			       (definition (and (pred1 implication negsucc)
						(pred2 implication negantecedent))))
		  (s-predicate (for implication)
			       (uses term)
			       (level 1)
			       (definition (term~alpha-equal implication (:param term))))
		  (c-predicate (for negantecedent term)
			       (multiple term)
			       (uses )
			       (exclude negsucc implication)
			       (level 1)
			       (definition (when (not (agplan~repeated-line-p (:node negantecedent)))
					     (pred3 (:node negantecedent)))))
		  (c-predicate (for negantecedent term)
			       (multiple term)
			       (uses negsucc)
			       (exclude implication)
			       (level 1)
			       (definition (pred4 (:node negantecedent) (:node negsucc))))
		  (c-predicate (for negantecedent)
			       (uses implication)
			       (exclude negsucc)
			       (level 1)
			       (definition (pred2 implication negantecedent)))
		  (s-predicate (for negsucc)
			       (uses implication)
			       (level 1)
			       (definition (pred1 implication negsucc)))
		  (function    (for term)
			       (uses implication)
			       (level 1)
			       (definition implication))
		  (function    (for term)
			       (uses negsucc negantecedent)
			       (exclude implication)
			       (level 1)
			       (definition (term~appl-create (logic~implication-constant)
							     (list negsucc negantecedent)))))
		 (predicates
		  (pred1 (a1 a2)
			 (and (logic~implication-p a1)
			      (term~alpha-equal a2 (nictac=logic-negate (second (data~appl-arguments a1))))))
		  (pred2 (a1 a2)
			 (and (logic~implication-p a1)
			      (term~alpha-equal a2 (nictac=logic-negate (first (data~appl-arguments a1))))))
		  (pred3 (a1)
			 (let ((res (mapcan
				     #'(lambda (term)
					 (when (logic~implication-p term)
					   (let ((uni (term~alpha-unify (first (data~appl-arguments term))
									(nictac=logic-negate (node~formula a1)))))
					     (when uni (list (subst~apply uni term))))))
				     (agplan~str-pos-subforms (pds~node-supports a1)))))
			   (when res (list res))))
		  (pred4 (a1 a2)
			 (let ((res (mapcan
				     #'(lambda (term)
					 (when (logic~implication-p term)
					   (let ((uni (term~alpha-unify term
									(term~appl-create
									 (logic~implication-constant)
									 (list (nictac=logic-negate (node~formula a1))
									       (nictac=logic-negate (node~formula a2)))))))
					     (when uni (list (subst~apply uni term))))))
				     (agplan~str-pos-subforms (pds~node-supports a1)))))
			   (when res (list res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
;;; Transitivity of implication: Modus Barbara
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

#|
(infer~deftactic NICTAC-modbarbara
		 (outline-mappings (((existent existent existent) NICTAC-modbarbara-a)
                                    ((nonexistent existent existent) NICTAC-modbarbara-f)
				    ((existent nonexistent existent) NICTAC-modbarbara-l)
				    ((existent existent nonexistent) NICTAC-modbarbara-r)))
                 (expansion-function batac=expand-NICTAC-modbarbara)
		 (help "Transitivity of Implication."))


(tac~deftactic NICTAC-modbarbara-f NICTAC-modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations (C (nictac=modbarbara-f (formula L1) (formula L2))))
   (sideconditions (nictac=modbarbara-f-p (formula L1) (formula L2)))
   (description "Transitivity of implication in forward direction."))

(defun nictac=NICTAC-modbarbara-f-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (term~alpha-equal (second (data~appl-arguments form1))
		   (first (data~appl-arguments form2)))))


(defun nictac=NICTAC-modbarbara-f (l1 l2)
  (let ((Opp (data~appl-function l1))
        (Aterm (first (data~appl-arguments l1)))
        (Bterm (second (data~appl-arguments l2))))
    ;; removing double negation
    (term~appl-create Opp (list Aterm Bterm))))

(tac~deftactic NICTAC-modbarbara-a NICTAC-modbarbara (in base)
   (premises L1 L2)
   (conclusions C)
   (computations )
   (sideconditions (nictac=NICTAC-modbarbara-a-p (formula C) (formula L1) (formula L2)))
   (description "Transitivity of implication."))

(tac~deftactic NICTAC-modbarbara-l NICTAC-modbarbara (in base)
	       (premises L1 L2)
	       (conclusions C)
	       (computations (L1 (nictac=NICTAC-modbarbara-l (formula C) (formula L2))))
	       (sideconditions (nictac=NICTAC-modbarbara-l-p (formula C) (formula L2)))
	       (description "Transitivity of implication in leftward direction."))

(defun nictac=NICTAC-modbarbara-l (c l2)
  (let ((Opp (data~appl-function c))
        (Aterm (first (data~appl-arguments c)))
        (Bterm (first (data~appl-arguments l2))))
    (term~appl-create Opp (list Aterm Bterm))))


(defun nictac=NICTAC-modbarbara-l-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (term~alpha-equal (second (data~appl-arguments form1))
		   (second (data~appl-arguments form2)))))


(tac~deftactic NICTAC-modbarbara-r NICTAC-modbarbara (in base)
	       (premises L1 L2)
	       (conclusions C)
	       (computations (L2 (nictac=NICTAC-modbarbara-r (formula C) (formula L1))))
	       (sideconditions (nictac=NICTAC-modbarbara-r-p (formula C) (formula L1)))
	       (description "Transitivity of implication in rightward direction."))

(defun nictac=NICTAC-modbarbara-r (c l1)
  (let ((Opp (data~appl-function c))
        (Aterm (second (data~appl-arguments l1)))
        (Bterm (second (data~appl-arguments c))))
    (term~appl-create Opp (list Aterm Bterm))))


(defun nictac=NICTAC-modbarbara-r-p (form1 form2)
  (and (logic~implication-p form1)
       (logic~implication-p form2)
       (term~alpha-equal (first (data~appl-arguments form1))
		   (first (data~appl-arguments form2)))))


(defun nictac=NICTAC-modbarbara-a-p (form1 form2 form3)
  (and (logic~implication-p form2)
       (logic~implication-p form2)
       (logic~implication-p form3)
       (term~alpha-equal (second (data~appl-arguments form2))
		   (first (data~appl-arguments form3)))
       (term~alpha-equal (nictac=NICTAC-modbarbara-f form2 form3)
		   form1)))


(defun nictac=expand-NICTAC-modbarbara (outline parameters)
  (let ((p1 (second outline))
	(p2 (third outline))
	(conc (first outline)))
    (tacl~init outline)
    (tacl~sequence
     (impi-res ('impi (list conc nil) nil))                ;;; conc  C hypA
     (BB ('impe (list nil (third impi-res) p1) nil))      ;;; B p1 hypA
     (CC ('impe (list (second impi-res) (car BB) p2) nil))) ;;; C p2 B
    (tacl~end)))

(defun nictac=NICTAC-modbarbara (line1 line2 line3)
  (infer~compute-outline 'NICTAC-modbarbara (list line1 line2 line3) nil))

(com~defcommand NICTAC-modbarbara
  (argnames conc line2 line3)
  (argtypes ndline ndline ndline)
  (arghelps "An implication line"
	    "Another one with the same antecedent"
	    "One with the same succedent as the first")
  (function nictac=NICTAC-modbarbara)
  (frag-cats tactics base)
  (defaults ((oc~default-current-planline) (com~unspecified) (com~unspecified)))
  (log-p T)
  (help "Transitivity of implication: modus barbara."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; And now ..... The MODBARB-AGENT !
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; --------------------------------------------------------
; MODUS BARBARA
; -------------------------------------------------------

;         line2   line3
;         A=>B     B=>C
;       -----------------
;            A => C
;            conc

(agent~defmatrix NICTAC-modbarbara
		 (agents (c-predicate (for conc)
				      (uses )
				      (exclude line2 line3)
				      (level 1)
				      (definition (logic~implication-p conc)))
			 (s-predicate (for line2)
				      (uses )
				      (exclude conc line3)
				      (level 1)
				      (definition (logic~implication-p line2)))
			 (s-predicate (for line3)
				      (uses )
				      (exclude conc line2)
				      (level 1)
				      (definition (logic~implication-p line3)))
			 (c-predicate (for conc)
				      (uses line2)
				      (exclude line3)
				      (level 1)
				      (definition (pred1 conc line2)))
			 (c-predicate (for conc)
				      (uses line3)
				      (exclude line2)
				      (level 1)
				      (definition (pred2 conc line3)))
			 (s-predicate (for line2)
				      (uses line3)
				      (exclude conc)
				      (definition (pred3 line2 line3)))
			 (s-predicate (for line3)
				      (uses line2)
				      (exclude conc)
				      (definition (pred3 line2 line3)))
			 (c-predicate (for conc)
				      (uses line2 line3)
				      (exclude )
				      (definition (pred4 conc line2 line3))))
		 (predicates
		  (pred1 (a1 a2)
			(and (logic~implication-p a1)
			     (logic~implication-p a2)
			(term~alpha-equal (first (data~appl-arguments a1))
				    (first (data~appl-arguments a2)))))
		  (pred2 (a1 a2)
			(and (logic~implication-p a1)
			     (logic~implication-p a2)
			     (term~alpha-equal (second (data~appl-arguments a1))
				    (second (data~appl-arguments a2)))))
		  (pred3 (a1 a2)
			 (and (logic~implication-p a1)
			      (logic~implication-p a2)
			      (term~alpha-equal (second (data~appl-arguments a1))
					  (first (data~appl-arguments a2)))))

		  (pred4 (c a1 a2)
			 (and (logic~implication-p a1)
			      (logic~implication-p a2)
			      (logic~implication-p c)
			      (term~alpha-equal (first (data~appl-arguments c))
					  (first (data~appl-arguments a1)))
			      (term~alpha-equal (second (data~appl-arguments c))
					  (second (data~appl-arguments a2)))
			      (term~alpha-equal (second (data~appl-arguments a1))
					  (first (data~appl-arguments a2)))))))


|#


