;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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




(mod~defmod EXTDELTA 
            :uses (cl data delta keim node pdsn pos post term termix)
            :documentation "Abstract datatype for the extended delta relation."
            :exports (
                      extdelta+delta-relation
                      extdelta+delta-relation-pair
                      
                      extdelta~add-node
                      extdelta~add-nodes-to-delta
                      extdelta~add-pair!
                      extdelta~clause
                      extdelta~clause-position
                      extdelta~create-delta-pair
                      extdelta~create-relation
                      extdelta~delta-node-of-formula
                      extdelta~delta-relation-p
                      extdelta~formula
                      extdelta~formula-position
                      extdelta~get-atom
                      extdelta~get-literals
                      extdelta~pdsnode
                      extdelta~relation-pair-p
                      extdelta~relation-pairs
                      extdelta~remove-pair!
                      extdelta~replace!
                      ))








;---------------------- The Datastructures for the extended Delta-Relation ------------------

(eval-when (load compile eval)
  (defclass extdelta+delta-relation (delta+relation)
    ()
    (:documentation "This is the class for the extended Delta Relation. It inherits all
slots from the class delta+relation.")))

(defun extdelta~delta-relation-p (object)
  (declare (edited  "31-JAN-1995")
	   (authors Acsehn)
	   (input "An OBJECT." )
	   (effect "None." )
	   (value "T, iff OBJECT is an extended Delta Relation." ))
  (typep object 'extdelta+delta-relation))

(defun extdelta~create-relation ()
  (declare (edited  "10-FEB-1993 19:48")
	   (authors ACSEHN)
	   (input "None."  )
	   (effect "None."  )
	   (value "An empty extended delta relation."  ))
  (make-instance 'extdelta+delta-relation :relation-pairs nil))

(defgeneric extdelta~relation-pairs (delta-relation)
  (declare (edited  "31-JAN-1995")
	   (authors Acsehn)
	   (input "A DELTA-RELATION."  )
	   (effect "None." )
	   (value "The relation pairs of DELTA-RELATION." ))
  (:method ((delta-relation extdelta+delta-relation))
	   (delta~relation-pairs delta-relation)))
				     

(defsetf extdelta~relation-pairs (delta-relation) (pair-list)
  `(setf (delta~relation-pairs ,delta-relation) ,pair-list))


; Delta Relation Pairs


(eval-when (load compile eval)
  (defclass extdelta+delta-relation-pair (delta+relation-pair)
    ((delta-node-of-formula
      :initarg :delta-node-of-formula
      :initform nil
      :accessor extdelta~delta-node-of-formula
      :documentation "In this slot the ND node to which the formula of this relation pair is assigned.")
     )
    (:documentation "This is the structure for the relation pair of the extended Delta
Relation. It inherits the following slots from delta+relation-pair:
delta-formula, delta-position-in-formula, delta-clause, and
delta-position-in-clause.")))

(defun extdelta~relation-pair-p (object)
  (declare (edited  "31-JAN-1995")
	   (authors Acsehn)
	   (input "An OBJECT."  )
	   (effect "None." )
	   (value "T, iff OBJECT is an extended Delta Relation Pair." ))
  (typep object 'extdelta+delta-relation-pair))

(defun extdelta~create-delta-pair (formula formula-position formula-node clause clause-position)
  (declare (edited  "31-JAN-1995")
	   (authors Acsehn)
	   (input "A FORMULA, a position in FORMULA, FORMULA-POSITION, an ND node"
		  "FORMULA is associated to, FORMULA-NODE, a CLAUSE, and a position in"
		  "CLAUSE, CLAUSE-POSITION." )
	   (effect "None." )
	   (value "An extended relation pair." ))
  (make-instance 'extdelta+delta-relation-pair
		 :delta-formula formula
		 :delta-position-in-formula formula-position
		 :delta-clause clause
		 :delta-position-in-clause clause-position
		 :delta-node-of-formula formula-node))


(defmethod print-object ((object extdelta+delta-relation-pair) stream)
  (format stream "(~A,~A,~A)--(<Clause ~A:~A>,~A)"
	  (delta~delta-formula object)
	  (delta~delta-position-in-formula object)
	  (extdelta~delta-node-of-formula object)
	  (keim~name (delta~delta-clause object))
	  (cl~literals (delta~delta-clause object))
	  ;;(delta~delta-clause object)
	  (delta~delta-position-in-clause object)))

	    
(defgeneric extdelta~formula (object)
  (declare (edited  "09-MAY-1995")
	   (authors Acsehn)
	   (input "An OBJECT."  )
	   (effect "None." )
	   (value "The formula of OBJECT." ))
  (:method ((pair extdelta+delta-relation-pair))
	   (delta~delta-formula pair)))

(defgeneric extdelta~formula-position (object)
  (declare (edited  "09-MAY-1995")
	   (authors Acsehn)
	   (input "An OBJECT." )
	   (effect "None." )
	   (value "The position of the formula in OBJECT." ))
  (:method ((pair extdelta+delta-relation-pair))
	   (delta~delta-position-in-formula pair)))

(defgeneric extdelta~pdsnode (object)
  (declare (edited  "09-MAY-1995")
	   (authors Acsehn)
	   (input "An OBJECT." )
	   (effect "None." )
	   (value "The ND node of OBJECT." ))
  (:method ((pair extdelta+delta-relation-pair))
	   (extdelta~delta-node-of-formula pair)))

(defgeneric extdelta~clause (object)
  (declare (edited  "09-MAY-1995")
	   (authors Acsehn)
	   (input "An OBJECT." )
	   (effect "None." )
	   (value "The clause of OBJECT." ))
  (:method ((pair extdelta+delta-relation-pair))
	   (delta~delta-clause pair)))

(defgeneric extdelta~clause-position (object)
  (declare (edited  "09-MAY-1995")
	   (authors Acsehn)
	   (input "An OBJECT." )
	   (effect "None." )
	   (value "The position of clause in OBJECT." ))
  (:method ((pair extdelta+delta-relation-pair))
	   (delta~delta-position-in-clause pair)))


;---------------- Algorithms for the extended delta relation --------------------


(defgeneric extdelta~add-pair! (extdelta-relation formula position formula-node clause position-of-literal)
  (declare (edited  "31-MAR-1993 16:42")
	   (authors ACSEHN)
	   (input "a delta relation, a named formula, a position of an"
		  "atomic occurrence of this formula,"
		  "and a position of the literal in the clause.")
	   (effect "a new relation pair is created and added to the delta-relation." )
	   (value "the altered delta relation."  ))
  (:method ((extdelta-relation extdelta+delta-relation)
	    (formula termix+named-term)
	    (position pos+position)
	    (formula-node pdsn+node)
	    (clause cl+clause)
	    (position-of-literal pos+position))
	   (let* ((relation-pairs (delta~relation-pairs extdelta-relation))
		  (new-pair (extdelta~create-delta-pair formula position formula-node clause position-of-literal))
		  (new-relation-pairs (cons new-pair relation-pairs)))
	     (setf (delta~relation-pairs extdelta-relation) new-relation-pairs)
	     extdelta-relation)))



(defgeneric extdelta~remove-pair! (delta-relation formula position pdsnode clause position-of-literal)
  (declare (edited  "31-MAR-1993 16:42")
	   (authors ACSEHN)
	   (input  "A delta relation."
		   "A named formula, a position of an atomar occurence in this formula,"
		   "a clause and a position of a literal in this clause.")
	   (effect "The relation pair with these contents is removed from the delta-relation." )
	   (value "The altered DELTA-RELATION."  ))
  (:method ((delta-relation extdelta+delta-relation)
	    (formula termix+named-term)
	    (position pos+position)
	    (pdsnode pdsn+node) 
	    (clause cl+clause)
	    (position-of-literal pos+position))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (pairs (mapcan #'(lambda (pair)
				     (if (and (keim~equal (delta~delta-formula pair) formula)
					      (keim~equal (delta~delta-position-in-formula pair) position)
					      (keim~equal (extdelta~delta-node-of-formula pair) pdsnode)
					      (eq (delta~delta-clause pair) clause)
					      (keim~equal (delta~delta-position-in-clause pair) position-of-literal))
					 (list pair)
					 NIL))
				 relation-pairs)))
	     (setf (delta~relation-pairs delta-relation) (set-difference relation-pairs pairs))
	     delta-relation)))


(defgeneric extdelta~replace! (object old-object new-object)
  (declare (edited  "03-JUN-1993 18:38")
	   (authors ACSEHN)
	   (input "an OBJECT, i.e. a delta relation and an OLD-OBJECT which can be a clause or a named term and"
		  "a NEW-OBJECT of the corresponding type.")
	   (effect "The OLD-OBJECT is replaced by the NEW-OBJECT in OBJECT."  )
	   (value "The altered OBJECT."  ))
  (:method ((relation extdelta+delta-relation) (old-object termix+named-term) (new-object termix+named-term))
	   (let ((subrelations (delta~get-subrelations relation)))
	     (mapcar #'(lambda (pair)
			 (let ((old-clause (delta~delta-clause pair))
			       (old-pos-in-clause (delta~delta-position-in-clause pair))
			       (old-pdsnode (extdelta~delta-node-of-formula pair))
			       (old-formula (delta~delta-formula pair))
			       (old-pos-in-formula (delta~delta-position-in-formula pair)))
			   (when (keim~equal old-formula old-object)
			     (progn (extdelta~remove-pair! relation old-formula
							   old-pos-in-formula
							   old-pdsnode
							   old-clause
							   old-pos-in-clause)
				    (extdelta~add-pair! relation new-object
							old-pos-in-formula
							old-pdsnode
							old-clause
							old-pos-in-clause)))))
		     subrelations)
	     relation))
  (:method ((relation extdelta+delta-relation) (old-object pdsn+node) (new-object pdsn+node))
	   (let ((subrelations (delta~get-subrelations relation)))
	     (mapcar #'(lambda (pair)
			 (let ((old-clause (delta~delta-clause pair))
			       (old-pos-in-clause (delta~delta-position-in-clause pair))
			       (old-pdsnode (extdelta~delta-node-of-formula pair))
			       (old-formula (delta~delta-formula pair))
			       (old-pos-in-formula (delta~delta-position-in-formula pair)))
			   (when (keim~equal old-pdsnode old-object)
			     (progn (extdelta~remove-pair! relation
							   old-formula
							   old-pos-in-formula
							   old-pdsnode
							   old-clause
							   old-pos-in-clause)
				    (extdelta~add-pair! relation
							old-formula
							old-pos-in-formula
							new-object
							old-clause
							old-pos-in-clause)))))
		     subrelations)
	     relation))
  (:method ((relation extdelta+delta-relation) (old-object cl+clause) (new-object cl+clause))
	   (let ((subrelations (delta~get-subrelations relation)))
	     (mapcar #'(lambda (pair)
			 (let ((old-clause (delta~delta-clause pair))
			       (old-pos-in-clause (delta~delta-position-in-clause pair))
			       (old-pdsnode (extdelta~delta-node-of-formula pair))
			       (old-formula (delta~delta-formula pair))
			       (old-pos-in-formula (delta~delta-position-in-formula pair)))
			   (when (eq old-clause old-object)
			     (progn (extdelta~remove-pair! relation
							   old-formula
							   old-pos-in-formula
							   old-pdsnode
							   old-clause
							   old-pos-in-clause)
				    (extdelta~add-pair! relation
							old-formula
							old-pos-in-formula
							old-pdsnode
							new-object
							old-pos-in-clause)))))
		     subrelations)
	     relation))
  (:method ((relation extdelta+delta-relation)
	    (old-object extdelta+delta-relation-pair)
	    (new-object extdelta+delta-relation-pair))
	   (let* ((relation-pairs (extdelta~relation-pairs relation))
		  (new-relpairs (mapcar #'(lambda (pair)
					    (if (keim~equal pair old-object)
						new-object
					      pair))
					relation-pairs)))
	     #+weg(setq nrps new-relpairs)
	     #+weg(break)
	     (setf (delta~relation-pairs relation) new-relpairs)
	     relation))
  (:method ((relation extdelta+delta-relation) (old-object list) (new-object list))
	   (mapcar #'(lambda (old new)
		       (extdelta~replace! relation old new))
		   old-object
		   new-object)
	   relation))


(defmethod keim~equal ((obj1 extdelta+delta-relation-pair) (obj2 extdelta+delta-relation-pair))
  (and
   (data~equal (extdelta~formula obj1) (extdelta~formula obj2))
   (keim~equal (extdelta~formula-position obj1)
	       (extdelta~formula-position obj2))
   (keim~equal (extdelta~pdsnode obj1)
	       (extdelta~pdsnode obj2))
   (eq (extdelta~clause obj1)
       (extdelta~clause obj2))
   (keim~equal (extdelta~clause-position obj1)
	       (extdelta~clause-position obj2))))
					   
(defgeneric extdelta~add-node (delta-relation-pair nd-node)
  (declare (edited  "31-JAN-1995")
	   (authors Acsehn)
	   (input "A DELTA-RELATION-PAIR, and an ND-NODE." )
	   (effect "None." )
	   (value "An extended Delta Relation pair where the old relation pair,"
		  "DELTA-RELATION-PAIR, is augemented by ND-NODE, i.e., the formula of"
		  "DELTA-RELATION-PAIR is assigned with ND-NODE." ))
  (:method ((pair delta+relation-pair) (node pdsn+node))
	   (let ((old-clause (delta~delta-clause pair))
		 (old-pos-in-clause (delta~delta-position-in-clause pair))
		 (old-formula (delta~delta-formula pair))
		 (old-pos-in-formula (delta~delta-position-in-formula pair)))
	     (extdelta~create-delta-pair
	      old-formula
	      old-pos-in-formula
	      node
	      old-clause
	      old-pos-in-clause))))

(defgeneric extdelta~add-nodes-to-delta (nd-nodes delta-relation)
  (declare (edited  "31-JAN-1995")
	   (authors Acsehn)
	   (input "A list of ND nodes, ND-NODES, and a DELTA-RELATION."  )
	   (effect "None." )
	   (value "An extended Delta Relation where the formulas in DELTA-RELATION are
assigned to the corresponding formulas of the ND nodes in ND-NODES."
		  ))
  (:method ((pdsnodes list) (delta-relation delta+relation))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (new-extdelta (extdelta~create-relation))
		  (new-relation-pairs
		   (mapcar #'(lambda (relation-pair)
			       (extdelta~add-node
				relation-pair
				(first (remove-if-not
					#'(lambda (node)
					    (let ((named-term (delta~delta-formula relation-pair)))
					      ;; prueft auf term und namens Gleichheit !
					      ;; Termgleichheit allein genuegt nicht ! -> mehrere gleiche Terme
					      ;; aber: man muss dafuer sorgen, dass die (named-)Terme und die Nodes gleich heissen !
					      ;; (passiert in ref2nd-main: res2nd=init-pds-from-res-proof
					      (and (data~equal (node~formula node) (termix~term named-term))
						   (equal (keim~name node) (keim~name named-term)))))
					; this findes the corresponding nd-node for the formula of a relation pair.
					pdsnodes))))
			   relation-pairs))
		  )
	     (setf (extdelta~relation-pairs new-extdelta) new-relation-pairs)
	     new-extdelta)))


(defgeneric extdelta~get-atom (delta-relation clause position)
  (declare (edited  "06-APR-1995")
	   (authors Acsehn)
	   (input "An extended delta relation, a clause and a position."  )
	   (effect "None." )
	   (value "The atomic subformular which with the literal specified by CLAUSE and POSITION"
		  "is linked in DELTA-RELATION1, i.e."
		  "1. the named formula and"
		  "2. the position of the atom."
		  "3. The ND node where formula stands in."))
  (:method ((delta-relation extdelta+delta-relation)
	    (clause cl+clause)
	    (position pos+position))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (pairs (mapcan #'(lambda (pair) (if (and (eq (delta~delta-clause pair) clause)
							   (keim~equal (delta~delta-position-in-clause pair) position))
						      (list pair)
						    NIL))
				 relation-pairs))
		  (formula-in (delta~delta-formula (first pairs)))
		  (pdsnode (extdelta~delta-node-of-formula (first pairs)))
		  (position-formula (delta~delta-position-in-formula (first pairs))))
	     (values formula-in position-formula pdsnode))))

(defmethod extdelta~get-literals ((delta-relation extdelta+delta-relation)
				  (formula term+term)
				  (position pos+position)
				  (pdsnode pdsn+node))
  (let* ((relation-pairs (delta~relation-pairs delta-relation))
	 (pairs (mapcan #'(lambda (pair) (if (and 
					      (data~equal (termix~term (delta~delta-formula pair)) formula)
					      (keim~equal (delta~delta-position-in-formula pair)
							  position)
					      (keim~equal (extdelta~delta-node-of-formula pair)
							  pdsnode))
					     (list pair)
					   NIL))
			relation-pairs))
	 (all-clauses (mapcar #'(lambda (pair) (delta~delta-clause pair)) pairs))
	 (all-positions (mapcar #'(lambda (pair) (delta~delta-position-in-clause pair)) pairs)))
    (values all-clauses all-positions)))

  
(defgeneric extdelta~get-literals (delta-relation formula position pdsnode)
  (declare (edited  "31-MAR-1993 16:45")
	   (authors ACSEHN)
	   (input   "Delta-relation, a named formula, a position and an ND node.")
	   (effect "none.")
	   (value   "1. a list of all clauses that are in delta-relation to the position in formula, and"
		    "2. a list of all literals that are in delta-relation to the position in formula."))
  (:method ((delta-relation extdelta+delta-relation)
	    (formula termix+named-term)
	    (position pos+position)
	    (pdsnode pdsn+node))
	   (let* ((relation-pairs (delta~relation-pairs delta-relation))
		  (pairs (mapcan #'(lambda (pair) (if (and 
						       (keim~equal (delta~delta-formula pair) formula)
						       (keim~equal (delta~delta-position-in-formula pair)
								   position)
						       (keim~equal (extdelta~delta-node-of-formula pair)
								   pdsnode))
						      (list pair)
						    NIL))
				 relation-pairs))
		  (all-clauses (mapcar #'(lambda (pair) (delta~delta-clause pair)) pairs))
		  (all-positions (mapcar #'(lambda (pair) (delta~delta-position-in-clause pair)) pairs)))
	     (values all-clauses all-positions))))

(defmethod post~print ((object extdelta+delta-relation) stream)
  (let ((relation-pairs (delta~relation-pairs object)))
    (format stream "(extdelta-relation ")
    (mapc #'(lambda (relation-pair)
	      (format stream " ")
	      (post~print relation-pair stream))
	  relation-pairs)
    (format stream ")")))

(defmethod post~print ((object extdelta+delta-relation-pair) stream)
  (format stream "(~S" (if (termix~named-term-p (delta~delta-formula object))
			   (keim~name (delta~delta-formula object))
			 (delta~delta-formula object)))
  (format stream " ")
  (format stream "~A" (keim~name (extdelta~delta-node-of-formula object)))
  (format stream " (")
  (post~print (delta~delta-position-in-formula object) stream)
  (format stream "(~A" (keim~name (delta~delta-clause object)))
  (format stream " ")
  (post~print (delta~delta-position-in-clause object) stream)
  (format stream ")))"))

(defmethod keim~copy ((object extdelta+delta-relation-pair) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep object x)) downto))
      object
    (extdelta~create-delta-pair (keim~copy (delta~delta-formula object)
					   :explode explode
					   :share share
					   :preserve preserve
					   :downto downto)
				(keim~copy (delta~delta-position-in-formula object)
					   :explode explode
					   :share share
					   :preserve preserve
					   :downto downto)
				(extdelta~delta-node-of-formula object)
				(keim~copy (delta~delta-clause object)
					   :explode explode
					   :share share
					   :preserve preserve
					   :downto downto)
				(keim~copy (delta~delta-position-in-clause object)
					   :explode explode
					   :share share
					   :preserve preserve
					   :downto downto))))

(defmethod keim~copy ((object extdelta+delta-relation) &key (explode :all-classes) share preserve downto)
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep object x)) downto))
      object
    (let* ((new-relation (extdelta~create-relation)))
      (setf (extdelta~relation-pairs new-relation)
	    (keim~copy (extdelta~relation-pairs object)
		       :explode explode
		       :share share
		       :preserve preserve
		       :downto downto)))))
