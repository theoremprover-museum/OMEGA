;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)


(mod~defmod R2NTOP 
            :uses (cl data delta env extdelta keim lit logic node omega ot pds pos post ref res subst tacl term termix)
            :documentation "A module containing all top classes and functions for the ref2nd-proceudure."
            :exports (
                      r2ntop+decompose-unit
                      r2ntop+flip-clause
                      r2ntop+paramod-clause
                      r2ntop+permutation-complete
                      r2ntop+permutation-literals
                      r2ntop+reflex-clause
                      r2ntop+trans-clause
                      r2ntop+transitivity-clause
                      
                      r2ntop~apply-tactic
                      r2ntop~clauses-and-steps-from-clause
                      r2ntop~clauses-equal-till-permutation
                      r2ntop~create-decompose-unit
                      r2ntop~create-flip-clause
                      r2ntop~create-paramod-clause
                      r2ntop~create-reflex-clause
                      r2ntop~create-trans-clause
                      r2ntop~create-transitivity-clause
                      r2ntop~decompose-unit-conclusion-node
                      r2ntop~decompose-unit-extdelta-relation
                      r2ntop~decompose-unit-integral-formulas
                      r2ntop~decompose-unit-nodes-to-decompose
                      r2ntop~decompose-unit-plco-pairs
                      r2ntop~decompose-unit-pre-selected-nodes
                      r2ntop~decompose-unit-ref-graph
                      r2ntop~get-link-and-contra-literals
                      r2ntop~get-link-with-literal
                      r2ntop~get-literal-parent-and-position
                      r2ntop~get-permutation-complete-parent
                      r2ntop~get-permutation-complete-position-pairs
                      r2ntop~get-permutation-parent
                      r2ntop~get-permutation-positions
                      r2ntop~insert-lemma-in-proof!
                      r2ntop~make-permutation-complete
                      r2ntop~make-permutation-literals
                      r2ntop~new-const
                      r2ntop~new-named-term
                      r2ntop~node-present-p
                      r2ntop~node-trivial-p
                      r2ntop~order-modulo-test
                      r2ntop~paramod-clause-paramod-position
                      r2ntop~position-of-literal
                      r2ntop~remove-list
                      r2ntop~term-at-position
                      r2ntop~trans-clause-ground-clauses
                      r2ntop~trans-clause-ground-subst
                      r2ntop~trans-clause-history
                      r2ntop~trans-clause-renaming
                      r2ntop~write-pds-in-file
                      
                      r2ntop*avoid-doubeling
                      r2ntop*check-flag
                      r2ntop*conclusion-just
                      r2ntop*current-resolution
                      r2ntop*integral-formulas
                      r2ntop*link-counter
                      r2ntop*new-constants
                      r2ntop*reach-sspu-style
                      r2ntop*refutation-graph
                      r2ntop*skolem-constants
                      r2ntop*sspu-style
                      r2ntop*term-name-counter
                      r2ntop*tertium-non-datur
                      r2ntop*trans-debug))

;;; The following functions are internal in other modules and should not be used:
;;; (cl=split-literals data=copy)

#| ------------------------------------------------- SOME GLOBAL VARIABLES ------------------------------------------- |#

(defvar r2ntop*current-resolution nil)              ;; the current resolution proof, used by all decomposition rules

(defvar r2ntop*refutation-graph nil)

(defvar r2ntop*avoid-doubeling nil)

(defvar r2ntop*skolem-constants nil)

(defvar r2ntop*check-flag 't)

(defvar r2ntop*sspu-style 'aut)

(defvar r2ntop*integral-formulas 't)

(defvar r2ntop*reach-sspu-style 'case)

(defvar r2ntop*tertium-non-datur nil)

(defvar r2ntop*conclusion-just nil)

(defvar r2ntop*term-name-counter 0)

(defvar r2ntop*link-counter 0)

(defvar r2ntop*trans-debug nil)
;; zur verstaerkten Ausgabe im Falle zum Debuggen
;; Moeglich sind: nil     -> keine Ausgabe
;;                full    -> volle Ausgabe
;;                t!=full -> nicht ganz soviel Ausgabe

(defvar r2ntop*new-constants nil)                 ;; new constanten (wir in atp benoetigt !)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Einstellungen fuer Lemmatisierung:

(defvar r2ntop*lemma 'constants) ;; MOMENTAN: Nur auf constants
;; Schalter: mit oder ohne lemmas, welche Art von Lemmata erlaubt
;; falls nil       -> keine Lemmatisiserung
;; falls constants -> Lemmatisierung nur uber Lemmas mit Skolem-Konstanten, nicht uber Lemmas mit Skolem-Funktionen
;; sonst           -> volle Lemmatisierung

(defvar r2ntop*lemma*relative-weight nil) ;; Das relative Gewicht, ab dem ein Lemma eingefuehrt wird !!

(defvar r2ntop*lemma-skolems nil)  ;; die Skolem-konstanten in den Lemmas

(defvar r2ntop*lemma-nodes nil) ;; die lemma-nodes

(defvar r2ntop*indirect-proof-forced nil) ;; muss indirekter Beweis gemacht werden nach lemmatisierung ?

(defvar r2ntop*indirect-proof nil) ;; soll per Eingabe ein indirekter BEweis erzeugt werden ?

#| ------------------------------------------------ Classes + Function on them --------------------------------------- |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; decompose-unit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load compile eval)
  (defclass r2ntop+decompose-unit (keim+object)
    ((ref-graph :initarg :ref-graph
		:initform nil
		:accessor r2ntop~decompose-unit-ref-graph
		:documentation "The refutation-graph.")
     (extdelta-relation :initarg :extdelta-relation
			:initform nil
			:accessor r2ntop~decompose-unit-extdelta-relation
			:documentation "The extdelta-relation")
     (plco-pairs :initarg :plco-pairs
		 :initform nil
		 :accessor r2ntop~decompose-unit-plco-pairs
		 :documentation "the polylink-compare-pairs of the refutation-graph.")
     (nodes-to-decompose :initarg :nodes-to-decompose
			 :initform nil
			 :accessor r2ntop~decompose-unit-nodes-to-decompose
			 :documentation "the nodes to decompose.")
     (conclusion-node :initarg :conclusion-node
		      :initform nil
		      :accessor r2ntop~decompose-unit-conclusion-node
		      :documentation "The conclusion-node.")
     (integral-formulas :initarg :integral-formulas
			:initform nil
			:accessor r2ntop~decompose-unit-integral-formulas
			:documentation "The integral formulas.")
     (pre-selected-nodes :initarg :pre-selected-nodes
			 :initform nil
			 :accessor r2ntop~decompose-unit-pre-selected-nodes
			 :documentation "The preselected nodes.")
     ))
  )


(defun r2ntop~create-decompose-unit (ref-graph extdelta-relation plco-pairs nodes-to-decompose
					       conclusion-node integral-formulas pre-selected-nodes)
  (make-instance 'r2ntop+decompose-unit
		 :ref-graph ref-graph
		 :extdelta-relation extdelta-relation
		 :plco-pairs plco-pairs
		 :nodes-to-decompose nodes-to-decompose
		 :conclusion-node conclusion-node
		 :integral-formulas integral-formulas
		 :pre-selected-nodes pre-selected-nodes
		 ))


(defmethod print-object ((object r2ntop+decompose-unit) stream)
  (format stream "(DECOM-UNIT: ~%<~% REF-GRAPH:")
  (if (not (equal r2ntop*trans-debug 'full))
      (format stream " |Ref-Graph|")
    (progn
      (format stream " |Ref-Graph with Clauses:")
      (mapcar #'(lambda (clause)
		  (format stream "~%   ")
		  (print-object clause stream)
		  (format stream " <plist :~A>" (keim~plist clause)))
	      (ref~clause-nodes (r2ntop~decompose-unit-ref-graph object)))))
  
  (format stream ",~% EXTDELTA-RELATION:")
  (if (not (equal r2ntop*trans-debug 'full))	      
      (format stream "~A" (r2ntop~decompose-unit-extdelta-relation object))
    (progn
      (format stream "PAIRS:")
      (mapcar #'(lambda (pair)
		  (format stream "~% ~A" pair))
	      (extdelta~relation-pairs (r2ntop~decompose-unit-extdelta-relation object)))))
  
  (format stream ",~% PLCO-PAIRS: ~A ,~% PRE-SELECT-nodes: ~A ,~% NODES-TO-DECOMP: ~A ,~% CONC-NODE: ~A ,~% integral-formulas: ~A~%>~%)"
	  (r2ntop~decompose-unit-plco-pairs object)
	  (r2ntop~decompose-unit-pre-selected-nodes object)
	  (r2ntop~decompose-unit-nodes-to-decompose object)
	  (r2ntop~decompose-unit-conclusion-node object)
	  (r2ntop~decompose-unit-integral-formulas object)))

#| ----------------------------------------------- permutation literals ----------------------------------------------- |#

(eval-when (load compile eval)
  (defclass r2ntop+permutation-literals (res+justification)
    ()))
  
(defun r2ntop~make-permutation-literals (parent position1 position2)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A parent clause, and two positions.")
	   (effect  "None.")
	   (value   "A r2ntop+permutation-literals justification, that means,"
		    "In the parent the literal at position1 becames to"
		    "position two."))
  (make-instance 'r2ntop+permutation-literals
		 :parents (list parent)
		 :renamings (list (subst~create () ()))
		 :positions (list position1 position2)
		 :unifier (subst~create () ())
		 :method 'permutation))

(defgeneric r2ntop~get-permutation-parent (node)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A clause or a justification of a clause.")
	   (effect  "None.")
	   (value   "If input accords to a permutation-clause, the car of parent-slot."))
  (:method ((node cl+clause))
	   (r2ntop~get-permutation-parent (node~justification node)))
  (:method ((node r2ntop+permutation-literals))
	   (first (res~justification-parents node))))

(defgeneric r2ntop~get-permutation-positions (node)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A clause or a justification of a clause.")
	   (effect  "None.")
	   (value   "If input accords to a permutation-clause, the positions slot."))
  (:method ((node cl+clause))
	   (r2ntop~get-permutation-positions (node~justification node)))
  (:method ((node r2ntop+permutation-literals))
	   (res~justification-positions node)))



(defmethod print-object ((object r2ntop+permutation-literals) stream)
  (let ((positions (r2ntop~get-permutation-positions object)))
    (format stream "~%In parent ~A, literal at position ~A to position ~A."
	    (r2ntop~get-permutation-parent object)
	    (first positions)
	    (second positions))))

(defmethod res~check-step (clause (just r2ntop+permutation-literals) &key (verbose nil))
  (let* ((literals (cl~literals clause))
	 (parent (first (res~justification-parents just)))
	 (positions (res~justification-positions just))
	 (parent-literals (cl~literals parent))
	 (pos1 (first positions))
	 (pos2 (second positions))
	 (lit-in-parent-at-pos1 (data~struct-at-position parent-literals pos1))
	 (lit-in-clause-at-pos2 (data~struct-at-position literals pos2))
	 (eq-flag (every #'(lambda (pair)
			     (keim~equal (first pair) (second pair)))
			 (mapcar #'(lambda (old-lit new-lit)
				     (list old-lit new-lit))
				 (cons lit-in-parent-at-pos1 (remove lit-in-parent-at-pos1 parent-literals))
				 (cons lit-in-clause-at-pos2 (remove lit-in-clause-at-pos2 literals))))))
    
    (when verbose
      (format t "~%Permutation literals clause ~A with just ~A" clause just)
      (format t "~%EQ-Flag: ~A" eq-flag))
    
    eq-flag))


#| ----------------------------------------------- permutation complete ----------------------------------------------- |#

(eval-when (load compile eval)
  (defclass r2ntop+permutation-complete (res+justification)
    ()))
  
(defun r2ntop~make-permutation-complete (parent pos-pair-list)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A parent clause, and a list of position pairs.")
	   (effect  "None.")
	   (value   "A r2ntop+permutation-complete justification, that means,"
		    "In the parent the literals are permutated, the literal at the first position in a"
		    "pair gets to the second position in the pair."))
  (make-instance 'r2ntop+permutation-complete
		 :parents (list parent)
		 :renamings (list (subst~create () ()))
		 :positions pos-pair-list
		 :unifier (subst~create () ())
		 :method 'permutation-complete))

(defgeneric r2ntop~get-permutation-complete-parent (node)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A clause or a justification of a clause.")
	   (effect  "None.")
	   (value   "If input accords to a permutation-clause, the car of parent-slot."))
  (:method ((node cl+clause))
	   (r2ntop~get-permutation-complete-parent (node~justification node)))
  (:method ((node r2ntop+permutation-complete))
	   (first (res~justification-parents node))))

(defgeneric r2ntop~get-permutation-complete-position-pairs (node)
  (declare (edited  "13-SEP-1996")
	   (authors Ameier)
	   (input   "A clause or a justification of a clause.")
	   (effect  "None.")
	   (value   "If input accords to a permutation-clause, the positions slot."))
  (:method ((node cl+clause))
	   (r2ntop~get-permutation-complete-position-pairs (node~justification node)))
  (:method ((node r2ntop+permutation-complete))
	   (res~justification-positions node)))

(defmethod print-object ((object r2ntop+permutation-complete) stream)
  (let ((positions (r2ntop~get-permutation-complete-position-pairs object)))
    (format stream "~%In parent ~A, permutation of literals: ~A"
	    (r2ntop~get-permutation-complete-parent object)
	    positions)))

(defmethod keim::res~check-step (clause (just r2ntop+permutation-complete) &key (verbose nil))
  (let* ((literals (cl~literals clause))
	 (parent (first (res~justification-parents just)))
	 (pos-pair-list (res~justification-positions just))
	 (parent-literals (cl~literals parent))
	 (re-ordered-parent-literals (do* ((i 0 (+ i 1))
					   (back-literals nil))
					 ((= (length parent-literals) i)
					  back-literals)
				       (let* ((ipos (pos~list-position (list i)))
					      (acc-pos-pair (find ipos pos-pair-list :test #'(lambda (pos pos-pair)
											       (keim~equal pos (second pos-pair)))))
					      (next-lit (data~struct-at-position parent-literals (first acc-pos-pair))))
					 (setq back-literals (append back-literals (list next-lit))))))
	 (eq-flag (every #'(lambda (pair)
			     (keim~equal (first pair) (second pair)))
			 (mapcar #'(lambda (old-lit new-lit)
				     (list old-lit new-lit))
				 re-ordered-parent-literals
				 literals))))

    (when verbose
      (format t "~%Permutation complete clause ~A with just ~A" clause just)
      (format t "~%EQ-Flag: ~A" eq-flag))

    eq-flag))


#| ----------------------------------------------- Transformation clause ----------------------------------------------- |#

(eval-when (load compile eval)
  (defclass r2ntop+trans-clause (cl+clause)
    ((ground-subst :initarg :ground-subst
		   :initform nil
		   :accessor r2ntop~trans-clause-ground-subst
		   :documentation "The ground-substitution of a transformation clause.")
     (history :initarg :history
	      :initform nil
	      :accessor r2ntop~trans-clause-history
	      :documentation "The history of a transformation clause.")
     (renaming :initarg :renaming
	       :initform nil
	       :accessor r2ntop~trans-clause-renaming
	       :documentation "The renaming of a transformation clause.")
     (ground-clauses :initarg :ground-clauses
		     :initform nil
		     :accessor r2ntop~trans-clause-ground-clauses
		     :documentation "The ground-clauses to a transformation clause."))))


(defun r2ntop~create-trans-clause (literals &key
					    justification name (name-prefix "Trans-") (weight 0) (clause-set nil)
					    (ground-subst nil) (history nil) (renaming nil) (ground-clauses nil))
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A list of literals and with keywords a justification ,a name and a name-prefix,"
		    "ground-substitution,renaming,history and ground-clauses.")
	   (effect  "None.")
	   (value   "A new trans clause with the input literals and the according justification."))
  (multiple-value-bind
      (pos-literals neg-literals cons-literals)
      (keim::cl=split-literals literals)
    (let ((clause (make-instance 'r2ntop+trans-clause
				 :literals literals
				 :pos-literals pos-literals
				 :neg-literals neg-literals
				 :constraints cons-literals
				 :weight weight
				 :clause-set clause-set
				 :renaming renaming
				 :ground-subst ground-subst
				 :history history
				 :ground-clauses ground-clauses
				 :name (cond ((numberp name) (format nil "~A" name))
					     ((null name) (format nil "~A~A" name-prefix (incf keim::cl*clause-counter)))
					     (t (string name)))
				 :justification justification))
	  (pos (pos~add-front -1)))
      (mapc #'(lambda (literal)
		(lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
	    literals)
      clause)))

(defmethod keim~copy ((clause r2ntop+trans-clause) &key (explode :all-classes) share preserve downto)
  (data~copy clause
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod keim::data=copy ((clause r2ntop+trans-clause) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep clause x)) downto))
      clause
    (let* ((literals-copy (keim::data=copy (cl~literals clause) renaming am explode share preserve downto))
	   (new-literals (first literals-copy))
	   (new-ren (second literals-copy))
	   (new-clause (r2ntop~create-trans-clause new-literals
						   :justification (keim~copy (node~justification clause)
									     :explode explode
									     :share share
									     :preserve preserve
									     :downto downto)	       
						   :clause-set nil
						   :weight (cl~weight clause)
						   :history (r2ntop~trans-clause-history clause)
						   :ground-clauses (r2ntop~trans-clause-ground-clauses clause)
						   :renaming (keim~copy (r2ntop~trans-clause-renaming clause)
									:downto '(data+primitive))
						   :ground-subst (keim~copy (r2ntop~trans-clause-ground-subst clause)
									    :downto '(data+primitive))
						   )))
      
      (list new-clause
	    new-ren))))

(defmethod print-object ((object r2ntop+trans-clause) stream)
  (format stream "~%<TRANS-CLAUSE ~A:" (keim~name object))
  (mapcar #'(lambda (lit)
	      (format stream " ~A" lit))
	  (cl~literals object))
  (format stream "~%   history: ~A" (r2ntop~trans-clause-history object))
  (format stream "~%   renaming: ~A" (r2ntop~trans-clause-renaming object))
  (format stream "~%   ground-subst: ~A" (r2ntop~trans-clause-ground-subst object))
  (format stream "~%   ground-clauses: ~A" (r2ntop~trans-clause-ground-clauses object))
  (format stream "~%   >"))

#| ---------------------------------------------- PARAMOD - CLAUSE ----------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass r2ntop+paramod-clause (r2ntop+trans-clause)
    ((paramod-position :initarg :paramod-position
		       :initform nil
		       :accessor r2ntop~paramod-clause-paramod-position
		       :documentation "The psramod-position of a paramod-clause."))))


(defun r2ntop~create-paramod-clause (literals &key
					      justification name (name-prefix "PARAMOD-") (weight 0) (clause-set nil)
					      (ground-subst nil) (history nil) (renaming nil) (ground-clauses nil)
					      (paramod-position nil))
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A list of literals and with keywords a justification ,a name and a name-prefix,"
		    "ground-substitution,renaming,history and ground-clauses.")
	   (effect  "None.")
	   (value   "A new paramodulation clause with the input literals amd the according justification."))
  (multiple-value-bind
      (pos-literals neg-literals cons-literals)
      (keim::cl=split-literals literals)
    (let ((clause (make-instance 'r2ntop+paramod-clause
				 :literals literals
				 :pos-literals pos-literals
				 :neg-literals neg-literals
				 :constraints cons-literals
				 :weight weight
				 :clause-set clause-set
				 :renaming renaming
				 :ground-subst ground-subst
				 :history history
				 :ground-clauses ground-clauses
				 :paramod-position paramod-position
				 :name (cond ((numberp name) (format nil "~A" name))
					     ((null name) (format nil "~A~A" name-prefix (incf keim::cl*clause-counter)))
					     (t (string name)))
				 :justification justification))
	  (pos (pos~add-front -1)))
      (mapc #'(lambda (literal)
		(lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
	    literals)
      clause)))

(defmethod keim~copy ((clause r2ntop+paramod-clause) &key (explode :all-classes) share preserve downto)
  (data~copy clause
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod keim::data=copy ((clause r2ntop+paramod-clause) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep clause x)) downto))
      clause
    (let* ((literals-copy (keim::data=copy (cl~literals clause) renaming am explode share preserve downto))
	   (new-literals (first literals-copy))
	   (new-ren (second literals-copy))
	   (new-clause (r2ntop~create-paramod-clause new-literals
						     :justification (keim~copy (node~justification clause)
									       :explode explode
									       :share share
									       :preserve preserve
									       :downto downto)
						     :clause-set nil
						     :weight (cl~weight clause)
						     :history (r2ntop~trans-clause-history clause)
						     :ground-clauses (r2ntop~trans-clause-ground-clauses clause)
						     :renaming (keim~copy (r2ntop~trans-clause-renaming clause)
									  :downto '(data+primitive))
						     :ground-subst (keim~copy (r2ntop~trans-clause-ground-subst clause)
									      :downto '(data+primitive))
						     :paramod-position (keim~copy (r2ntop~paramod-clause-paramod-position clause)))))
      
      (list new-clause
	    new-ren))))

(defmethod print-object ((object r2ntop+paramod-clause) stream)
  (format stream "~%<PARAMOD-CLAUSE ~A:" (keim~name object))
  (mapcar #'(lambda (lit)
	      (format stream " ~A" lit))
	  (cl~literals object))
  (format stream "~%   history: ~A" (r2ntop~trans-clause-history object))
  (format stream "~%   renaming: ~A" (r2ntop~trans-clause-renaming object))
  (format stream "~%   ground-subst: ~A" (r2ntop~trans-clause-ground-subst object))
  (format stream "~%   ground-clauses: ~A" (r2ntop~trans-clause-ground-clauses object))
  (format stream "~%   paramod-position: ~A" (r2ntop~paramod-clause-paramod-position object))
  (format stream "~%   >"))


#| -------------------------------------------------- FLIP CLause ---------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass r2ntop+flip-clause (r2ntop+trans-clause)
    ()))

(defun r2ntop~create-flip-clause (literals &key
					   justification name (name-prefix "FLIP-") (weight 0) (clause-set nil)
					   (ground-subst nil) (history nil) (renaming nil) (ground-clauses nil))
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A list of literals and with keywords a justification ,a name and a name-prefix,"
		    "ground-substitution,renaming,history and ground-clauses.")
	   (effect  "None.")
	   (value   "A new flip clause with the input literals amd the according justification."))
  (multiple-value-bind
      (pos-literals neg-literals cons-lits)
      (keim::cl=split-literals literals)
    (let ((clause (make-instance 'r2ntop+flip-clause
				 :literals literals
				 :pos-literals pos-literals
				 :neg-literals neg-literals
				 :constraints cons-lits
				 :weight weight
				 :clause-set clause-set
				 :renaming renaming
				 :ground-subst ground-subst
				 :history history
				 :ground-clauses ground-clauses
				 :name (cond ((numberp name) (format nil "~A" name))
					     ((null name) (format nil "~A~A" name-prefix (incf keim::cl*clause-counter)))
					     (t (string name)))
				 :justification justification))
	  (pos (pos~add-front -1)))
      (mapc #'(lambda (literal)
		(lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
	    literals)
      clause)))

(defmethod keim~copy ((clause r2ntop+flip-clause) &key (explode :all-classes) share preserve downto)
  (data~copy clause
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod keim::data=copy ((clause r2ntop+flip-clause) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep clause x)) downto))
      clause
    (let* ((literals-copy (keim::data=copy (cl~literals clause) renaming am explode share preserve downto))
	   (new-literals (first literals-copy))
	   (new-ren (second literals-copy)))
      (list (r2ntop~create-flip-clause new-literals
				       :justification (keim~copy (node~justification clause)
								 :explode explode
								 :share share
								 :preserve preserve
								 :downto downto)	       
				       :clause-set nil
				       :weight (cl~weight clause)
				       :history (r2ntop~trans-clause-history clause)
				       :ground-clauses (r2ntop~trans-clause-ground-clauses clause)
				       :renaming (keim~copy (r2ntop~trans-clause-renaming clause)
							    :downto '(data+primitive))
				       :ground-subst (keim~copy (r2ntop~trans-clause-ground-subst clause)
								:downto '(data+primitive))
				       )
	    new-ren))))

(defmethod print-object ((object r2ntop+flip-clause) stream)
  (format stream "~%<FLIP-CLAUSE ~A:" (keim~name object))
  (mapcar #'(lambda (lit)
	      (format stream " ~A" lit))
	  (cl~literals object))
  (format stream "~%   history: ~A" (r2ntop~trans-clause-history object))
  (format stream "~%   renaming: ~A" (r2ntop~trans-clause-renaming object))
  (format stream "~%   ground-subst: ~A" (r2ntop~trans-clause-ground-subst object))
  (format stream "~%   ground-clauses: ~A" (r2ntop~trans-clause-ground-clauses object))
  (format stream "~%   >"))

#| ---------------------------------------------- Reflexivity-Clause -------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass r2ntop+reflex-clause (r2ntop+trans-clause)
    ()))
  
(defun r2ntop~create-reflex-clause (literals &key
					     justification name (name-prefix "Reflex-") (weight 0) (clause-set nil)
					     (ground-subst nil) (history nil) (renaming nil) (ground-clauses nil))
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A list of literals and with keywords a justification ,a name and a name-prefix,"
		    "ground-substitution,renaming,history and ground-clauses.")
	   (effect  "None.")
	   (value   "A new reflexivity clause with the input literals amd the according justification."))
  (multiple-value-bind
      (pos-literals neg-literals cons-literals)
      (keim::cl=split-literals literals)
    (let ((clause (make-instance 'r2ntop+reflex-clause
				 :literals literals
				 :pos-literals pos-literals
				 :neg-literals neg-literals
				 :constraints cons-literals
				 :weight weight
				 :clause-set clause-set
				 :renaming renaming
				 :ground-subst ground-subst
				 :history history
				 :ground-clauses ground-clauses
				 :name (cond ((numberp name) (format nil "~A" name))
					     ((null name) (format nil "~A~A" name-prefix (incf keim::cl*clause-counter)))
					     (t (string name)))
				 :justification justification))
	  (pos (pos~add-front -1)))
      (mapc #'(lambda (literal)
		(lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
	    literals)
      clause)))

(defmethod keim~copy ((clause r2ntop+reflex-clause) &key (explode :all-classes) share preserve downto)
  (data~copy clause
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod keim::data=copy ((clause r2ntop+reflex-clause) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep clause x)) downto))
      clause
    (let* ((literals-copy (keim::data=copy (cl~literals clause) renaming am explode share preserve downto))
	   (new-literals (first literals-copy))
	   (new-ren (second literals-copy)))
      (list (r2ntop~create-reflex-clause new-literals
					 :justification (keim~copy (node~justification clause)
								   :explode explode
								   :share share
								   :preserve preserve
								   :downto downto)	       
					 :clause-set nil
					 :weight (cl~weight clause)
					 :history (r2ntop~trans-clause-history clause)
					 :ground-clauses (r2ntop~trans-clause-ground-clauses clause)
					 :renaming (keim~copy (r2ntop~trans-clause-renaming clause)
							      :downto '(data+primitive))
					 :ground-subst (keim~copy (r2ntop~trans-clause-ground-subst clause)
								  :downto '(data+primitive))
					 )
	    new-ren))))

(defmethod print-object ((object r2ntop+reflex-clause) stream)
  (format stream "~%<REFLEX-CLAUSE ~A:" (keim~name object))
  (mapcar #'(lambda (lit)
	      (format stream " ~A" lit))
	  (cl~literals object))
  (format stream "~%   history: ~A" (r2ntop~trans-clause-history object))
  (format stream "~%   renaming: ~A" (r2ntop~trans-clause-renaming object))
  (format stream "~%   ground-subst: ~A" (r2ntop~trans-clause-ground-subst object))
  (format stream "~%   ground-clauses: ~A" (r2ntop~trans-clause-ground-clauses object))
  (format stream "~%   >"))

#| ---------------------------------------------- Transitivity-Clause -------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass r2ntop+transitivity-clause (r2ntop+trans-clause)
    ()))
  
(defun r2ntop~create-transitivity-clause (literals &key
						   justification name (name-prefix "Trans-") (weight 0) (clause-set nil)
						   (ground-subst nil) (history nil) (renaming nil) (ground-clauses nil))
  (declare (edited  "21-Aug-1997")
	   (authors Ameier)
	   (input   "A list of literals and with keywords a justification ,a name and a name-prefix,"
		    "ground-substitution,renaming,history and ground-clauses.")
	   (effect  "None.")
	   (value   "A new transitivity clause with the input literals and the according justification."))
  (multiple-value-bind
      (pos-literals neg-literals cons-lits)
      (keim::cl=split-literals literals)
    (let ((clause (make-instance 'r2ntop+transitivity-clause
				 :literals literals
				 :pos-literals pos-literals
				 :neg-literals neg-literals
				 :constraints cons-lits
				 :weight weight
				 :clause-set clause-set
				 :renaming renaming
				 :ground-subst ground-subst
				 :history history
				 :ground-clauses ground-clauses
				 :name (cond ((numberp name) (format nil "~A" name))
					     ((null name) (format nil "~A~A" name-prefix (incf keim::cl*clause-counter)))
					     (t (string name)))
				 :justification justification))
	  (pos (pos~add-front -1)))
      (mapc #'(lambda (literal)
		(lit~add-clause-position! literal clause (keim~copy (pos~next! pos))))
	    literals)
      clause)))

(defmethod keim~copy ((clause r2ntop+transitivity-clause) &key (explode :all-classes) share preserve downto)
  (data~copy clause
	     :explode explode
	     :share share
	     :preserve preserve
	     :downto downto))

(defmethod keim::data=copy ((clause r2ntop+transitivity-clause) renaming am explode share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep clause x)) downto))
      clause
    (let* ((literals-copy (keim::data=copy (cl~literals clause) renaming am explode share preserve downto))
	   (new-literals (first literals-copy))
	   (new-ren (second literals-copy)))
      (list (r2ntop~create-transitivity-clause new-literals
					       :justification (keim~copy (node~justification clause)
									 :explode explode
									 :share share
									 :preserve preserve
									 :downto downto)	       
					       :clause-set nil
					       :weight (cl~weight clause)
					       :history (r2ntop~trans-clause-history clause)
					       :ground-clauses (r2ntop~trans-clause-ground-clauses clause)
					       :renaming (keim~copy (r2ntop~trans-clause-renaming clause)
								    :downto '(data+primitive))
					       :ground-subst (keim~copy (r2ntop~trans-clause-ground-subst clause)
									:downto '(data+primitive))
					       )
	    new-ren))))

(defmethod print-object ((object r2ntop+transitivity-clause) stream)
  (format stream "~%<TRANSITIVITY-CLAUSE ~A:" (keim~name object))
  (mapcar #'(lambda (lit)
	      (format stream " ~A" lit))
	  (cl~literals object))
  (format stream "~%   history: ~A" (r2ntop~trans-clause-history object))
  (format stream "~%   renaming: ~A" (r2ntop~trans-clause-renaming object))
  (format stream "~%   ground-subst: ~A" (r2ntop~trans-clause-ground-subst object))
  (format stream "~%   ground-clauses: ~A" (r2ntop~trans-clause-ground-clauses object))
  (format stream "~%   >"))


#| --------------------------------------------------- SOME AUXILIARY Functions ---------------------------------------------- |#

#| -> use instead remove-duplicates

(defun r2ntop~remove-doubles (liste &key (test 'eq))
  (declare (edited  "23-FEB-1996")
	   (authors Ameier)
	   (input   "A list and a keyword test (default eq) for the"
		    "check-predicat.")
	   (effect  "None.")
	   (value   "The list after deleting doubles according to the"
		    "check-predicat for doubles given by test."))
  (reverse (r2ntop=apply-union (mapcar 'list (reverse liste)) :test test)))

(defun r2ntop=apply-union (list-of-lists &key (test 'eq))
  (declare (edited  "22-FEB-1996")
	   (authors Ameier)
	   (input   "A list of lists and a keyword test for the equality"
		    "check-funktion (default eq)")
	   (effect  "None.")
	   (value   "The list of the union of all lists according to"
		    "the check-predicat test."))
  (cond ((= (length list-of-lists) 0)
	 nil)
	((= (length list-of-lists) 1)
	 (car list-of-lists))
	(t 
	 (let ((head (first list-of-lists))
	       (tail (rest list-of-lists)))
	   (union head (r2ntop=apply-union tail :test test) :test test)))))
|#

(defun r2ntop~remove-list (list-to-remove list-from-remove &key (test 'eq))
  (declare (edited  "25-MAR-1996")
	   (authors Ameier)
	   (input   "A list1 of items to remove from a list2 of items and a keyword"
		    "test (default eq) for the check-function.")
	   (effect  "None.")
	   (value   "The list of items from list2 after all items of list1 are removed." ))
  (if list-to-remove
      (let ((head (first list-to-remove))
	    (tail (rest list-to-remove)))
	(r2ntop~remove-list tail (remove head list-from-remove) :test test))
    list-from-remove))

#| -> vielleicht besser als ^
 
(defun r2ntop~remove-list (list-to-remove list-from-remove &key (test 'eq))
  (declare (edited  "25-MAR-1996")
	   (authors Ameier)
	   (input   "A list1 of items to remove from a list2 of items and a keyword"
		    "test (default eq) for the check-function.")
	   (effect  "None.")
	   (value   "The list of items from list2 after all items of list1 are removed." ))
  (remove-if #'(lambda (item)
		 (find item list-to-remove :test test))
	     list-from-remove))
|#

(data~defgeneric r2ntop~term-at-position ((object) position)
  (declare (edited  "27-NOV-1996")
	   (authors Ameier)
	   (input   "A term or list of terms and a position.")
	   (effect  "None.")
	   (value   "The (list) of subtems of OBJECT at the position POSITION."
		    "In difference to data~struct-at-position this function handles"
		    "(equiv A B) as (and (implies A B) (implies B A)")
	   (example "(QQ Y Y) [2] --> Y"))
  (:method ((object-list list) position)
   (if (pos~empty-p position)
       object-list
       (r2ntop~term-at-position (elt object-list (pos~first position)) (pos~rest position))))
  (:method ((term term+term) position)
   (if (pos~empty-p position)
       term
     (let ((number (pos~first position))
	   (subterms (data~substructs term)))
       (if (> number (length subterms))
	   (error "Position ~A does not exist in term ~A." position term)
	 (if (not (logic~equivalence-p term))
	     (r2ntop~term-at-position (elt subterms number) (pos~rest position))
	   (let* ((arguments (data~appl-arguments term))
		  (implies (env~lookup-object 'implies (pds~environment omega*current-proof-plan)))
		  (and (env~lookup-object 'and (pds~environment omega*current-proof-plan)))
		  (subterms (list and
				  (term~appl-create implies arguments)                       
				  (term~appl-create implies (reverse arguments)))))          
	       (r2ntop~term-at-position (elt subterms number) (pos~rest position))))))))
  (:method ((literal lit+literal) position)
	   (cond (( pos~empty-p position) literal)
		 ((= 1 (pos~first position)) (r2ntop~term-at-position (lit~atom literal) (pos~rest position)))
		 (t (error "Position ~A does not exist in literal ~A." position literal))))
  (:method ((clause cl+clause) position)
	   (r2ntop~term-at-position (cl~literals clause) position)))

(defgeneric r2ntop~clauses-and-steps-from-clause (clause just)
  (declare (edited  "22-FEB-1996")
	   (authors Ameier)
	   (input   "A clause and its justification.")
	   (effect  "None.")
	   (value   "Multiple-value with:"
		    "First: The list of all initial clauses in the "
		    "       reolution-tree contained in the justification"
		    "       and clause itself,if initial clause."
		    "Second: The list of all not initial clauses"
		    "        (step-clauses) in the resolution-tree contained"
		    "        in the justification and clause itself, if"
		    "        step-clause."))
  (:method ((clause cl+clause) (just res+resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (parent1 (first parents))
		  (parent2 (second parents))
		  (just1 (node~justification parent1))
		  (just2 (node~justification parent2)))
	     (multiple-value-bind
		 (initial-clauses1 steps1)
		 (r2ntop~clauses-and-steps-from-clause parent1 just1)
	       (multiple-value-bind
		   (initial-clauses2 steps2)
		   (r2ntop~clauses-and-steps-from-clause parent2 just2)
		 (values (append initial-clauses1 initial-clauses2)
    			 (cons clause (append steps1 steps2)))))))
  (:method ((clause cl+clause) (just r2ntop+permutation-literals))
	   (let* ((parent (r2ntop~get-permutation-parent just))
		  (parent-just (node~justification parent)))
	     (multiple-value-bind
		 (initial-clauses steps)
		 (r2ntop~clauses-and-steps-from-clause parent parent-just)
	       (values initial-clauses (cons clause steps)))))
  (:method ((clause cl+clause) (just r2ntop+permutation-complete))
	   (let* ((parent (r2ntop~get-permutation-complete-parent just))
		  (parent-just (node~justification parent)))
	     (multiple-value-bind
		 (initial-clauses steps)
		 (r2ntop~clauses-and-steps-from-clause parent parent-just)
	       (values initial-clauses (cons clause steps)))))
  (:method ((clause cl+clause) (just res+hyper-resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (clauses-and-steps-lists (mapcar #'(lambda (parent)
						       (multiple-value-bind
							   (initials steps)
							   (r2ntop~clauses-and-steps-from-clause parent
											       (node~justification parent))
							 (list initials steps)))
						   parents)))
	     (values (apply 'append (mapcar #'first clauses-and-steps-lists))
		     (cons clause (apply 'append (mapcar #'second clauses-and-steps-lists))))))
  (:method ((clause cl+clause) (just res+ur-resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (clauses-and-steps-lists (mapcar #'(lambda (parent)
						       (multiple-value-bind
							   (initials steps)
							   (r2ntop~clauses-and-steps-from-clause parent
											       (node~justification parent))
							 (list initials steps)))
						   parents)))
	     (values (apply 'append (mapcar #'first clauses-and-steps-lists))
		     (cons clause (apply 'append (mapcar #'second clauses-and-steps-lists))))))
  (:method ((clause cl+clause) (just res+paramodulation))
	   (let* ((mother (res~paramod-mother just))
		  (father (res~paramod-father just))
		  (mother-just (node~justification mother))
		  (father-just (node~justification father)))
	     (multiple-value-bind
		 (initial-mother steps-mother)
		 (r2ntop~clauses-and-steps-from-clause mother mother-just)
	       (multiple-value-bind
		   (initial-father steps-father)
		   (r2ntop~clauses-and-steps-from-clause father father-just)
		 (values (append initial-mother initial-father)
			 (cons clause (append steps-father steps-mother)))))))
  (:method ((clause cl+clause) (just res+factoring))
	   (let* ((parent (res~factoring-clause just))
		  (parent-just (node~justification parent)))
	     (multiple-value-bind
		 (initial-clauses steps)
		 (r2ntop~clauses-and-steps-from-clause parent parent-just)
	       (values initial-clauses (cons clause steps)))))
  (:method ((clause cl+clause) (just res+initial))
	   (values (list clause) nil)))

(defun r2ntop~order-modulo-test (liste &key (test 'eq))
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A list and a keyword test (default eq).")
	   (effect  "None.")
	   (value   "A list of lists, each sub-list consisting of all elements of"
		    "of the input-list who are equal according to the test-function."))
  (do* ((return-list nil (let ((head (first rest-list)))
			   (cons (cons head (remove-if-not #'(lambda (item)
							       (apply test (list head item)))
							   (rest rest-list))) return-list)))     
	(rest-list liste (remove-if #'(lambda (item)
					(apply test (list (first rest-list) item)))
				    (rest rest-list))))
      ((null rest-list) return-list)))

#|(defun r2ntop~get-literal-parent-and-position (literal)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "A list consisting of the clause of the literal and the position"
		    "of the literal in this clause."))
  (let ((clause (lit~clause literal)))
    (list
     clause
     (r2ntop=position-of-literal-in-clause literal clause))))


(defun r2ntop=position-of-literal-in-clause (literal clause)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A literal and its clause.")
	   (effect  "None.")
	   (value   "the position of the literal in the clause."))
  (let ((list-of-literals (cl~literals clause)))
    (do* ((lits list-of-literals (rest lits))
	  (current-lit (first list-of-literals) (first lits))
	  (pos 0 (+ pos 1)))
	((or (eq current-lit literal) (null lits))
	 (if (eq current-lit literal)
	     (pos~list-position (list pos))
	   nil))))) |#

(defun r2ntop~get-literal-parent-and-position (literal)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "A list consisting of the clause of the literal and the position"
		    "of the literal in this clause."))
  (let ((clause-position (lit~clause-position literal)))
    (if (not (= (length clause-position) 1))
	(omega~error "Problem in Funktion r2ntop~get-literal-parent-and-position")
      (list (car (first clause-position))
	    (rest (first clause-position))))))


#| (defun r2ntop~position-of-literal (literal list-of-literals)
  (declare (edited  "21-MAR-1996")
	   (authors Ameier)
	   (input   "A literal and a list of literals (like from clauses)")
	   (effect  "None.")
	   (value   "The position of the literal in the list-of-literals,if"
		    "it is in, otherwise nil."))
  (do* ((lits list-of-literals (rest lits))
	(current-lit (first list-of-literals) (first lits))
	(pos 0 (+ pos 1)))
      ((or (eq current-lit literal) (null lits))
       (if (eq current-lit literal)
	   (pos~list-position (list pos))
	 nil)))) |#

(defun r2ntop~position-of-literal (literal list-of-literals)
  (declare (edited  "21-MAR-1996")
	   (authors Ameier)
	   (input   "A literal and a list of literals (like from clauses)")
	   (effect  "None.")
	   (value   "The position of the literal in the list-of-literals,if"
		    "it is in, otherwise nil."))
  (declare (ignore list-of-literals))
  (second (r2ntop~get-literal-parent-and-position literal)))

(defun r2ntop~write-pds-in-file (pds file &key (supersede t))
  (with-open-file (out file :direction :output 
		       :if-exists (if supersede :supersede :append)
		       :if-does-not-exist :create)
		  (post~print pds out))
  (format nil "Wrote pds ~A to file ~A." (keim~name pds) file))       ;; keine Ahnung ob das noch Funktioniert AMEIER


(defun r2ntop~apply-tactic (tactic outline parameters)
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "Das ist nur ein Scheissding um")
	   (effect  "Das tacl~apply besser tracen zu koennen")
	   (value   "Ne"))

  (when r2ntop*trans-debug
    (omega~message "Calling tacl~~apply with ~A ~A ~A" tactic outline parameters))
  
  (tacl~apply tactic outline parameters))

(defun r2ntop~node-trivial-p (node)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A pdsnode.")
	   (effect  "None.")
	   (value   "T if the pdsnode is trivial, i.e. its formula is a (possibly negated) atom."
		    "nil otherwise."))
  (let* ((formula (node~formula node)))
    (if (or (logic~atom-p formula)
	    (and (logic~negation-p formula)
		 (logic~atom-p (first (data~appl-arguments formula)))))
	t
      nil)))

(defun r2ntop~node-present-p (node delta-relation)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A pdsnode and a delta-relation.")
	   (effect  "None.")
	   (value   "T if the pdsnode occurs in some pair of the delta-relation."
		    "Nil otherwise."))
  (let ((delta-pairs (extdelta~relation-pairs delta-relation)))
    (remove-if-not #'(lambda (pair)
		       (eq (extdelta~pdsnode pair) node))
		   delta-pairs)))



(defun r2ntop~clauses-equal-till-permutation (clause1 clause2)
  (let ((literals1 (cl~literals clause1))
	(literals2 (cl~literals clause2))
	(flag t))
    (if (not (= (length literals1) (length literals2)))
	nil
      (progn
	(loop while (and literals1 flag) do	  
	      (let* ((head (first literals1))
		     (head-in-literals2 (remove-if-not #'(lambda (x)
							   (keim~equal x head)) literals2))
		     (not-head-in-literals2 (remove-if #'(lambda (x)
							   (keim~equal x head)) literals2)))
		(setq literals1 (rest literals1))
		(if head-in-literals2
		    (setq literals2 (append (rest head-in-literals2) not-head-in-literals2))
		  (setq flag nil))))
	(if (and flag (not literals2))
	    t
	  nil)))))                      


#| ---------------------------------------------- Computing new constants in the environment ---------------------------------------- |#

(defun r2ntop~new-const (old-thing)
  (declare (edited  "21-MAR-1996")
	   (authors Ameier)
	   (input   "A variable.")
	   (effect  "A new constant of the same type as variable is created and"
		    "entered in the environment of omega*current-proof-plan."
		    "Additionally it is added to the r2ntop*new-constants list.")
	   (value   "The new constant."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (new-const (term~generate-term-primitive-with-new-name 'c (term~type old-thing) 'term+constant env)))
    (setq r2ntop*new-constants (cons new-const r2ntop*new-constants))
    new-const))

    
#| ---------------------------------------------- Computing new nmaed terms --------------------------------------------------------- |#

(defun r2ntop~new-named-term (term)
  (declare (edited  "09-MAY-1995")
	   (authors AMEIER)
	   (input "An (unnamed) TERM." )
	   (effect "None." )
	   (value "A named term with TERM as term entry." ))
  (make-instance 'termix+named-term
		 :name (intern (make-symbol (format nil "TERM-~A" (incf r2ntop*term-name-counter))))
		 :term term))

#| ----------------------------------------------- Handling literals and contras ------------------------------------------------- |#

(defun r2ntop~get-link-and-contra-literals (literal links)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A literal and a list of links.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: The link from list-of-links whose shores contain literal,"
		    "       nil, if no such link exists."
		    "Second: If first is nil, nil too."
		    "        Otherwise the list of literals that stand in the contrary-shore"
		    "        of the literal in the link from First."))
  (let ((link (r2ntop~get-link-with-literal literal links)))
    (if link
	(let ((pos-shore (ref~positive-shore link))
	      (neg-shore (ref~negative-shore link)))
	  (if (member literal pos-shore)
	      (values link neg-shore)
	    (values link pos-shore)))
      (values nil nil))))

(defun r2ntop~get-link-with-literal (literal links)
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A literal and a list of links.")
	   (effect  "None.")
	   (value   "The link whose shores contain literal."
		    "If no such link exists nil is returned."))
  (first (remove-if-not #'(lambda (link)
			    (member literal (append (ref~positive-shore link)
						    (ref~negative-shore link))))			
			links)))


#| -------------------------------------------------- INSERT LEMMA IN PROOF --------------------------------------------------------- |#

(defun r2ntop~insert-lemma-in-proof! (link-to-expand ref-graph delta-relation)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A link, the refutation-graph containing this link, the according"
		    "delta-relation and conclusion-node.")
	   (effect  "A new lemma is introduced in omega*current-proof-plan by using the"
		    "two (ground) formulas of the shores of the link."
		    "This lemma looks like:  (Hyps-of-conclusion-node) |- A v -A (LEMMA)."
		    "Accoding to this new lemma a new clause [A,-A] is produced and inserted"
		    "into the refutation-graph by adding in the input link."
		    "this looks like:"
		    " ...,-A]--*--[A,...   becames to: ...,-A]--*--[A,-A]--*--[A,...  "
		    "by this the links are updated too."
		    "According to this new clause the delta-relation is updated.")
	   (value   "The new-inserted Lemma pdsnode."))
  (let* ((pos-shore (ref~positive-shore link-to-expand))
	 (neg-shore (ref~negative-shore link-to-expand))
	 (pos-lit (r2ntop=get-ground-literal (first pos-shore)))
	 (neg-lit (r2ntop=get-ground-literal (first neg-shore))))
    (multiple-value-bind
	(new-clause new-pdsnode new-delta-pairs)
	(r2ntop=create-new-trivial-hyp pos-lit neg-lit)
      (setf (ref~links ref-graph) (remove link-to-expand (append (list
								  (ref~link-create
								   (intern (string-upcase (format nil "split-link-~A"
												  (incf r2ntop*link-counter)))
									   (find-package :omega))
								   (mapcar #'r2ntop~get-literal-parent-and-position
									   (cons neg-lit pos-shore))
								   (subst~create () ()))
								  (ref~link-create 
								   (intern (string-upcase (format nil "split-link-~A"
												  (incf r2ntop*link-counter)))
									   (find-package :omega))
								   (mapcar #'r2ntop~get-literal-parent-and-position
									   (cons pos-lit neg-shore))
								   (subst~create () ())))
								 (ref~links ref-graph)))
	    )
      (setf (delta~relation-pairs delta-relation)
	    (append new-delta-pairs (extdelta~relation-pairs delta-relation)))
      (setf (ref~clause-nodes ref-graph)
	    (cons new-clause (ref~clause-nodes ref-graph)))
      new-pdsnode)))

(defun r2ntop=get-ground-literal (literal)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A literal (possibly not ground).")
	   (effect  "None.")
	   (value   "The according ground-literal."
		    "Computed by taking the ground-substitution of the literals clause"
		    "and applying it to literal."))
  (let* ((clause (lit~clause literal))
	 (ground-subst (r2ntop~trans-clause-ground-subst clause)))
    (lit~literal-create (subst~apply ground-subst (lit~atom literal)) (lit~positive-p literal))))

(defun r2ntop=create-new-trivial-hyp (pos-lit neg-lit)
  (declare (edited  "01-AUG-1996")
	   (authors Ameier)
	   (input   "A positive ground-literal, the according negative ground-literal.")
	   (effect  "With this literals (call them A -A) a clause is produced."
		    "A new lemma is introduced into omega*current-proof-plan by using the"
		    "two formulas of this clause. This lemma looks like:"
		    " (Hyps-of-conclusion-node) |- A v -A (LEMMA).")
	   (value   "Multiple-values:"
		    "First: The new-clause"
		    "Second: The according new lemma pdsnode"
		    "Third: A list of the corresponding two new delta-pairs."))
  (let* ((new-clause (r2ntop~create-trans-clause (list pos-lit neg-lit)
						 :ground-subst (subst~create () ())
						 :renaming (subst~create () ())))
	 (atom (lit~atom pos-lit))
	 (formula atom))
    (when (null r2ntop*tertium-non-datur)
      (setq r2ntop*tertium-non-datur
	    (pds~add-thy-assertion (ot~read-thy-assumption 'tertium-non-datur)
				   omega*current-proof-plan)))
    (let* ((new-node (first (r2ntop~apply-tactic 'foralle (list nil r2ntop*tertium-non-datur) (list formula)))))
      
      (keim~put new-node 'trivial-hyp 't)
      
      (values
       new-clause new-node (list (extdelta~create-delta-pair formula
							     (pos~list-position '(1))
							     new-node
							     new-clause
							     (pos~list-position '(0)))
				 (extdelta~create-delta-pair formula
							     (pos~list-position '(2))
							     new-node
							     new-clause
							     (pos~list-position '(1))))))))
    
	
