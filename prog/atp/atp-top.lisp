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


(mod~defmod atptop :uses (res node post env prob pds pdsn just keim mixin agent)
	    :documentation "Top modul of atp system."
	    :exports (
		      atptop~get-default-directory
		      atptop~default-directory
		      
		      atptop~order-resolution-steps!
		      
		      atptop~resolution-proof-create-from-pds-open-node
		      atptop~resolution-proof-create-from-nodes
		      
		      atptop~get-ancestors

		      atptop~construct-and-add-problem!
		      atptop~remove-list
		      atptop~read-problem
		      
		      atptop~insert-sub-proof-in-original-proof!

		      atptop~compute-label-counter

		      atptop~filter-chars
		      atptop~cut-first-char
		      atptop~cut-last-char
		      atptop~get-next-word
		      atptop~number-of-char-in-string
		      atptop~divide-string-by-lists
		      atptop~divide-string
		      atptop~char2number
		      atptop~string-is-prefix-of-string-p

		      atptop~clauses-equal-till-renaming-p
		      atptop~clauses-equal-till-renaming-and-ordering-p
		      atptop~parse-only-numbers
		      atptop~call-with-time-ressource
		      atptop~get-pid-from-stream
		      atptop~get-pids-of-children
		      atptop~kill
		      atptop~filter-resolution-proof

		      atptop~prover-finished-p
		       
		      atptop~read-file-as-string
		      atptop~print-string-in-file

		      atptop~copy-res-proof
		      atptop~check-p2f-p
		      atptop~equation-p


		      atptop~subset-by-equality-except-names-p
		      atptop~detect-atp
		      atptop~change-resolution-proof!
		      atptop~remove-clauses-with-abstractions! 
		      )
	    )



;; a counter to count the new produced problems
(defvar atptop*new-problem-counter 0)

;; a flag, whether interactive things are possible or not (in expansion are sometimes all interactive things unliked)
(defvar atptop*interactivity-allowed 't)

;; a flag to sign whether each atp output should be parsed back
(defvar atptop*parse-all 't)

#| ----------------------------------------------- Detecting whcih ATP created a File -------------------------------------------- |#

(defun atptop~detect-atp (file)
  (with-open-file (stream file
			  :direction :input)
		  (let* ((first-line (read-line stream nil '+-*/))
			 (second-line (read-line stream nil '+-*)))
		    (cond ((atptop~string-is-prefix-of-string-p "----- Otter" first-line)
			   'otter)
			  ((atptop~string-is-prefix-of-string-p "----- EQP" first-line)
			   'eqp)
			  ((atptop~string-is-prefix-of-string-p "Bliksem" first-line)
			   'bliksem)
			  ((atptop~string-is-prefix-of-string-p "***************************************************" first-line)
			   'waldmeister)
			  ((atptop=detect-protein file)
			   'protein)
			  ((atptop=detect-spass file)
			   'spass)
			  (t
			   nil)))))

(defun atptop=detect-spass (file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (all-lines (atptop~divide-string out-string #\Newline)))

    (find "SPASS" all-lines :test #'atptop~string-is-prefix-of-string-p)))

(defun atptop=detect-protein (file)
  (let* ((out-string (atptop~read-file-as-string file))
	 (all-lines (atptop~divide-string out-string #\Newline)))

    (find "********************    PROTEIN" all-lines :test #'atptop~string-is-prefix-of-string-p)))

#| -------------------------------------------- Check whether clause sets are subsets ---------------------------------------------- |#

;; The following stuff is needed to read the output files of an ATP without the same resolution proof in hand and without
;; the convert-list to read match back the objects form the outpuit file
;; Hence, the idea is to create a new resolution proof with new clauses and new objects, and to read then the input clauses
;; of the out file and to create new constant + variable objects and with these new objects again new clauses.
;; Then the two clause sets are mapped in the way that for each clause in the set of the read clauses there is a clause
;; in the initisal clause set of the new resolution proof. This mapping maps each object in the read clauses to an object
;; in the initial clauses. Finally, from this mapping a new convert-list is computed, mapping the names in the ATP out-file
;; to objects in the new resolution proof.
;;
;; NOTE: This approach suffers currently under one problem:
;;       We can parse-back clauses only if the literals are in the same order as in the clauses of the new resolution proof.
;;
;;

(defun atptop~subset-by-equality-except-names-p (cl-list1 cl-list2 &key (current-mapping (mapp~create nil nil)))
  (declare (edited  "23-MAY-2000")
	   (authors Ameier)
	   (input   "Two lists of clauses.")
	   (effect  "None.")
	   (value   "Multiple-Value:"
		    "First: Success flag (t/nil). T there exists a mapping of constants to constants and variables to"
		    "       variables, such that for each clause in the frist clause-set exists a corresponding"
		    "       clause in the second clause set, such that the clauses are equal modulo this mapping."
		    "Second: If first is t the described mapping, otherwise nil."
		    "Third: If first is t a list of triples of two clauses and a list of position pairs."
		    "       Thereby, the first clause is from the first clause set and the second clause is a corresponding"
		    "       matchable clause from the second clause set, respectively. The position pair list describes which"
		    "       literal of the first clause is matched to which literal of the second clause."))
  (if (null cl-list1)
      (values 't current-mapping nil)
    (let* ((head-clause (first cl-list1)))
      (multiple-value-bind
	  (list-of-possible-corresponding-clauses list-of-possible-mappings list-of-possible-position-pairs)
	  (atptop=compute-possible-correspondig-clauses-and-mappings head-clause cl-list2 :current-mapping current-mapping)
	
	(if list-of-possible-corresponding-clauses
	    (do* ((rest-list-poss-corr-cl list-of-possible-corresponding-clauses (rest rest-list-poss-corr-cl))
		  (rest-list-poss-mapp list-of-possible-mappings (rest rest-list-poss-mapp))
		  (rest-list-poss-position-pairs list-of-possible-position-pairs (rest rest-list-poss-position-pairs))
		  (success-flag nil)
		  (resulting-mapping nil)
		  (resulting-pair-list nil))
		((or (null rest-list-poss-corr-cl)
		     success-flag)
		 (if success-flag
		     (values 't
			     resulting-mapping
			     resulting-pair-list)
		   (values nil nil nil)))
	      (let* ((head-poss-cl (first rest-list-poss-corr-cl))
		     (head-poss-mapp (first rest-list-poss-mapp))
		     (head-poss-position-pairs (first rest-list-poss-position-pairs)))
		
		(when (null (atptop=not-correct-mapping-p head-poss-mapp))
		  (multiple-value-bind
		      (success result-mapp pairs)
		      (atptop~subset-by-equality-except-names-p (rest cl-list1) cl-list2
								:current-mapping (mapp~create (mapp~domain head-poss-mapp)
											      (mapp~codomain head-poss-mapp)))
		    
		    (when success
		      (setf success-flag 't)
		      (setf resulting-mapping result-mapp)
		      (setf resulting-pair-list (cons (list head-clause head-poss-cl head-poss-position-pairs) pairs)))))))
	  (values nil nil nil))))))

(defun atptop=not-correct-mapping-p (mapping)
  (declare (edited  "01-JUN-2000")
	   (authors Ameier)
	   (input   "A mapping.")
	   (effect  "None.")
	   (value   "A mapping is incorrect for our purposes, if its codomain contains the object twice."))
  (let* ((codomain (mapp~codomain mapping)))
    (if (< (length (remove-duplicates codomain)) (length codomain))
	't
      nil)))    

(defun atptop=compute-possible-correspondig-clauses-and-mappings (cl cl-list &key (current-mapping (mapp~create nil nil)))
  (declare (edited  "23-MAY-2000")
	   (authors Ameier)
	   (input   "A clause and a list of clasues. Furthermore, via a keyword a mapping.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of clauses of the clause list, such that the input clause can be mapped to these clauses."
		    "Second: A list of corresponding mappings."
		    "Third: A list of corresponding literal positions mappings (expressed as pairs of positions)."))
  (do* ((rest-cl-list cl-list (rest rest-cl-list))
	(back-clauses nil)
	(back-mappings nil)
	(back-position-pairs-lists nil))
      ((null rest-cl-list)
       (values back-clauses
	       back-mappings
	       back-position-pairs-lists))
    (let* ((head-clause (first rest-cl-list)))
      
      (multiple-value-bind
	  (success resulting-mappings resulting-pair-lists)
	  (atptop=mapp-literals (cl~literals cl) (cl~literals head-clause)
				:current-mapping (mapp~create (mapp~domain current-mapping)
							      (mapp~codomain current-mapping)))
	
	;; it can happens, that we get for one clause several possible mappings and possible literal matchings! 
	(when success
	  (let* ((needed-clauses (do* ((rest-mappings resulting-mappings (rest rest-mappings))
				       (back-cls nil (cons head-clause back-cls)))
				     ((null rest-mappings)
				      back-cls))))
	    ;; fore each possible matching add the head clause once
	    (setf back-clauses (append needed-clauses back-clauses))
	    (setf back-mappings (append resulting-mappings back-mappings))
	    (setf back-position-pairs-lists (append (mapcar #'(lambda (literal-pair-list)
								(atptop=compute-position-pair-list literal-pair-list cl head-clause))
							    resulting-pair-lists)
						    back-position-pairs-lists))))))))

(defun atptop=compute-position-pair-list (literal-pair-list cl1 cl2)
  (declare (edited  "26-JUN-2000")
	   (authors Ameier)
	   (input   "A list of literal pairs and two clauses, such that the first literal of a literal pair is in the"
		    "first clause and the second literal is in the second clause.")
	   (effect  "None.")
	   (value   "A corresponding list of position pairs."))
  (mapcar #'(lambda (literal-pair)
	      (let* ((lit1 (first literal-pair))
		     (lit2 (second literal-pair)))
		(list (pos~list-position (list (position lit1 (cl~literals cl1))))
		      (pos~list-position (list (position lit2 (cl~literals cl2)))))))
	  literal-pair-list))

(defun atptop=compute-possible-correspondig-literals-and-mappings (lit lit-list &key (current-mapping (mapp~create nil nil)))
  (declare (edited  "23-MAY-2000")
	   (authors Ameier)
	   (input   "A literal and a list of literals. Furthermore, via a keyword a mapping.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: A list of literals of the literal list, such that the input literal can be mapped to these literals."
		    "Second: A list of corresponding mappings."))
  (do* ((rest-lit-list lit-list (rest rest-lit-list))
	(back-literals nil)
	(back-mappings nil))
      ((null rest-lit-list)
       (values back-literals back-mappings))
    (let* ((head-lit (first rest-lit-list)))
      
      (multiple-value-bind
	  (success resulting-mapping)
	  (atptop=mapp-objects lit head-lit :current-mapping (mapp~create (mapp~domain current-mapping)
									  (mapp~codomain current-mapping)))
	
	(when success
	  (setf back-literals (cons head-lit back-literals))
	  (setf back-mappings (cons resulting-mapping back-mappings)))))))

(defun atptop=mapp-literals (literals1 literals2 &key (current-mapping (mapp~create nil nil)))
  (declare (edited  "26-JUN-2000")
	   (authors Ameier)
	   (input   "Two list of literals and as a keyword the current mapping.")
	   (effect  "NOne.")
	   (value   "Tries to maps the literals of the first list to literals of the second list."
		    "Multiple-value:"
		    "First: Success Flag (t/nil). T if there exists a mapping of constants to constants and variables to"
		    "       variables, such that for each literal in the first literal-list exists a corresponding"
		    "       literal in the second literal-list, such that the literals are equal modulo this mapping."
		    "Second: A list of corresponding mappings."
		    "Third: A list of lists of literal pairs. Each literal pair list describes a possible literal matching."))
  (cond ((and (null literals1)
	      (null literals2))
	 (values 't
		 (list current-mapping)
		 nil))
	((null (= (length literals1)
		  (length literals2)))
	 (values nil nil nil))
	(t
	 (let* ((head-lit (first literals1)))
	   (multiple-value-bind
	       (list-of-possible-corresponding-literals list-of-possible-mappings)
	       (atptop=compute-possible-correspondig-literals-and-mappings head-lit literals2 :current-mapping current-mapping)
	     
	     (if list-of-possible-corresponding-literals
		 (do* ((rest-list-poss-corr-lits list-of-possible-corresponding-literals (rest rest-list-poss-corr-lits))
		       (rest-list-poss-mapp list-of-possible-mappings (rest rest-list-poss-mapp))
		       (resulting-pair-list-list nil)
		       (resulting-mapping-list nil))
		     ((null rest-list-poss-corr-lits)
		      (if resulting-mapping-list
			  (values 't
				  resulting-mapping-list
				  resulting-pair-list-list)
			(values nil nil nil)))
		   (let* ((head-poss-lit (first rest-list-poss-corr-lits))
			  (head-poss-mapp (first rest-list-poss-mapp)))
		     
		     (when (null (atptop=not-correct-mapping-p head-poss-mapp))
		       (multiple-value-bind
			   (success result-mapps result-pairs-lists)
			   (atptop=mapp-literals (rest literals1) (remove head-poss-lit literals2)
						 :current-mapping (mapp~create (mapp~domain head-poss-mapp)
									       (mapp~codomain head-poss-mapp)))
			 
			 (when success
			   (setf success-flag 't)
			   (setf resulting-mapping-list (append result-mapps resulting-mapping-list))
			   (setf resulting-pair-list-list (append (if result-pairs-lists
								      (mapcar #'(lambda (pair-list)
										  (cons (list head-lit head-poss-lit) pair-list))
									      result-pairs-lists)
								    (list (list (list head-lit head-poss-lit))))
								  resulting-pair-list-list)))))))
	       (values nil nil nil)))))))

(defgeneric atptop=mapp-objects (obj1 obj2 &key (current-mapping (mapp~create nil nil)))
  (declare (edited  "23-MAY-2000")
	   (authors Ameier)
	   (input   "Two objects (clause, literal, term, lists) and via keyword a current mapping.")
	   (effect  "None.")
	   (value   "MUltiple-value:"
		    "First: Success flag (t/nil), t if the first object can be mapped on the second object."
		    "Second: if first is t the corresponding mapping."))
  (:method ((lit1 lit+literal) (lit2 lit+literal) &key (current-mapping (mapp~create nil nil)))
	   (if (equal (lit~polarity lit1) (lit~polarity lit2))
	       (atptop=mapp-objects (lit~atom lit1) (lit~atom lit2)
				    :current-mapping (mapp~create (mapp~domain current-mapping)
								  (mapp~codomain current-mapping)))
	     (values nil nil)))
  (:method ((appl1 term+appl) (appl2 term+appl) &key (current-mapping (mapp~create nil nil)))
	   (atptop=mapp-objects (cons (data~appl-function appl1) (data~appl-arguments appl1))
			    (cons (data~appl-function appl2) (data~appl-arguments appl2))
			    :current-mapping (mapp~create (mapp~domain current-mapping)
							  (mapp~codomain current-mapping))))
  (:method ((prim1 term+primitive) (prim2 term+primitive) &key (current-mapping (mapp~create nil nil)))
	   (if (or (and (term~variable-p prim1) (term~constant-p prim2))
		   (and (term~variable-p prim2) (term~constant-p prim1))
		   )
	       (values nil nil)
	     (let* ((assoc-list (mapcar #'list (mapp~domain current-mapping) (mapp~codomain current-mapping)))
		    (find-prim1-in-mapping? (assoc prim1 assoc-list :test #'keim~equal)))
	       (if find-prim1-in-mapping?
		   ;; prim1 already bound in mapping on a constant prim1prime -> check whether prim1prime = prim2 
		   (let* ((prim1prime (second find-prim1-in-mapping?)))
		     (if (keim~equal prim1prime prim2)
			 (values 't current-mapping)
		       (values nil nil)))
		 ;; prim1 not bound so far -> bound prim1 on prim2
		 (values 't (mapp~create (cons prim1 (mapp~domain current-mapping))
					 (cons prim2 (mapp~codomain current-mapping))))))))
  (:method ((list1 list) (list2 list)  &key (current-mapping (mapp~create nil nil)))
	   (if (null (= (length list1) (length list2)))
	       (values nil nil)
	     (do* ((rest-list1 list1 (rest rest-list1))
		   (rest-list2 list2 (rest rest-list2))
		   (flag t)
		   (mapp current-mapping))
		 ((or (null rest-list1)
		      (null flag))
		  (if (null flag)
		      (values nil nil)
		    (values 't mapp)))
	       (let* ((head-list1 (first rest-list1))
		      (head-list2 (first rest-list2)))
		 (multiple-value-bind
		     (success result-mapp)
		     (atptop=mapp-objects head-list1 head-list2 :current-mapping (mapp~create (mapp~domain mapp)
											  (mapp~codomain mapp)))
		   
		   (if success
		       (setf mapp result-mapp)
		     (setf flag nil)))))))
  (:method (obj1 obj2 &key (current-mapping (mapp~create nil nil)))
	   (values nil nil)))


(defun atptop~change-resolution-proof! (res-proof clauses-triples)
  (declare (edited  "26-JUN-2000")
	   (authors Ameier)
	   (input   "A resolution proof and a list of triples, consisting of two clauses and a list of position pairs."
		    "Thereby, the second clause is a clause from the input clauses of the resolution proof and the"
		    "position pairs describe a bijective mapping from literals of the frist clause to this second clause.")
	   (effect  "1.) The clauses of the resolution proof are changed in the following way:"
		    "    They are ordered in the way their corresponding literals in the first triple clause are ordered."
		    "2.) In the delta-relation pairs the clause positions are also changed accordingly.")
	   (value   "The changed resolution proof."))
  (let* ((delta-relation (res~proof-delta-relation res-proof))
	 (delta-pairs (delta~relation-pairs delta-relation)))
    
    (mapcar #'(lambda (clauses-triple)
		(let* ((cl1 (first clauses-triple))
		       (cl2 (second clauses-triple))
		       (position-pairs (third clauses-triple))
		       (delta-pairs-to-clause-cl2 (remove-if-not #'(lambda (delta-pair)
								     (eq (delta~delta-clause delta-pair) cl2))
								 delta-pairs)))

		  (atptop=change-clause-order! cl2 position-pairs)

		  (mapcar #'(lambda (delta-pair)
			      (atptop=change-delta-relation-pair! delta-pair position-pairs))
			  delta-pairs-to-clause-cl2)))
	    clauses-triples)))

(defun atptop=change-delta-relation-pair! (delta-pair position-pairs)
  (declare (edited  "26-JUN-2000")
	   (authors Ameier)
	   (input   "A delta relation pair to a clause cl and a list of position pairs describing a bijective mapping"
		    "of the literals of another clause to the literals of the input clause. This bijective mapping can"
		    "also be seen as a reordering of the literals of the clause.")
	   (effect  "The position-in-clause of the delta-relation pair is changed accordingly to this positions-pair"
		    "list. For instance, if the position-in-clause is (2) and the positions-pair list contains a"
		    "pair ((1) (2)), then position-in-clause gets (1), etc. ")
	   (value   "The changed delta-pair."))
  (let* ((pos-in-clause (delta~delta-position-in-clause delta-pair))
	 (new-pos (first (find pos-in-clause position-pairs :test #'(lambda (pos pos-pair)
								      (keim~equal pos (second pos-pair)))))))

    (setf (delta~delta-position-in-clause delta-pair) new-pos)
    delta-pair))

(defun atptop=change-clause-order! (clause position-pairs)
  (declare (edited  "26-JUN-2000")
	   (authors Ameier)
	   (input   "A clause and a list of position pairs. Thereby, the position pairs describe a bijective mapping"
		    "of the literals of another clause to the literals of the input clause.")
	   (effect  "The literal list of the clause is changed by re-ordering the literals of the clause, in a way corresponding"
		    "to the order of the literals in the other clause. This is done in the following way: the new first literal"
		    "gets that literal at the position p if the pair ((0) p) is in the positions pair list, etc. ")
	   (value   "The changed clause."))
  (let* ((literals (cl~literals clause)))
    
    (do* ((rest-position-pairs position-pairs)
	  (curr-pos-num 0 (+ curr-pos-num 1))
	  (back-literals nil))
	((null rest-position-pairs)
	 (progn (setf (cl~literals clause) back-literals)
		clause))
      (let* ((next-pair (find curr-pos-num rest-position-pairs :test #'(lambda (num pos-pair)
									 (= num (first (pos~number-list (first pos-pair)))))))
	     (pos (second next-pair))
	     (next-lit (data~struct-at-position literals pos)))
	
	(setq rest-position-pairs (remove next-pair rest-position-pairs))
	(setq back-literals (append back-literals (list next-lit)))))))  

#| ---------------------------------------------------- Default Directory -------------------------------------------------------- |#


(defun atptop~default-directory ()
  (declare (edited  "02-MAR-1999" "28-OCT-1997")
	   (authors Pollet Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "The default directory for temporary files."
		    "This function doesn't create the directory."))
  (let* ((default-dir (mk::append-directories user::*dir-sep* "tmp"
					      user::*dir-sep* (format nil "~A-atp-dir" (sys~getenv "USER"))
					      user::*dir-sep* )))
    default-dir ))


(defun atptop~get-default-directory ()
  (declare (edited  "28-OCT-1997")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "The default directory for temporary files."))
  (let* ((default-dir (mk::append-directories user::*dir-sep* "tmp"
					      user::*dir-sep* (format nil "~A-atp-dir" (sys~getenv "USER"))
					      user::*dir-sep* )))

    (when (not (probe-file default-dir))
      (sys~call-system (format nil "mkdir ~A" default-dir)))
    default-dir))

      
#| --------------------------------------------------- Order resolution steps ---------------------------------------------------- |#

(defun atptop~order-resolution-steps! (resolution-proof)
  (declare (edited  "09-DEC-1996")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "The sequence of steps in the proofs step slot is changed in a way,"
		    "that the last step is the empty clause and no new step stands before"
		    "its parents.")
	   (value   "The resolution proof."))
  (let* ((empty-clause (res~proof-empty-clause resolution-proof))
	 (initial-clauses (res~proof-initial-clauses resolution-proof))
	 (steps (atptop~get-ancestors empty-clause (node~justification empty-clause) nil)))

    (mapcar #'(lambda (step)
		(keim~remprop step 'ancestors))
	    steps)
    
    (setf (res~proof-clauses resolution-proof) (append initial-clauses steps))
    resolution-proof))

(defgeneric atptop~get-ancestors (clause just initials)
  (declare (edited  "09-DEC-1996")
	   (authors Ameier)
	   (input   "A clause and its justification a flag, to deceide, whether"
		    "the initial ancestors should be returned to or not (default is nil).")
	   (effect  "REMARK: In all clauses the plist is changed by a entry ancestors.")
	   (value   "A list of all its ancestors, that are not initial clauses and the"
		    "clause itself as the last element of the list."
		    "This list is in the right order in the sence, that no clause is"
		    "in it before some of its ancestor clauses."))
  (:method (clause (just res+initial) initials)
	   (if initials
	       (list clause)
	     nil))
  (:method (clause (just res+reflex) initials)
	   (declare (ignore initials))
	   (list clause))
  (:method (clause just initials)

	   (let* ((ancestors (keim~get clause 'ancestors)))
	     (if ancestors
		 ancestors ;; wenn clause bereits abgearbeitet -> ancestors in plist gesetzt -> zurueck
	       ;; wenn nicht -> berechnen und eintragen
	       (let* ((parents (res~justification-parents just))
		      (new-ancestors
		       (remove-duplicates
			(append (apply 'append (mapcar #'(lambda (parent)
							   (atptop~get-ancestors parent (node~justification parent) initials))
						       parents))
				(list clause))
			:from-end 't)))
		 (keim~put clause 'ancestors new-ancestors)
		 new-ancestors)))))

#| ------------------------------------------------- Create initial resolution proof --------------------------------------------- |#

;; Nach simplification check in atp.lisp can man davon ausgehen, dass (zumindest) die CONCLUSION kein SCHEMA mehr ist!!

(defun atptop~resolution-proof-create-from-pds-open-node (open-node pds)
  (declare (edited  "24-JUN-1997")
	   (authors Ameier)
	   (input   "An open node and the pds of this open node.")
	   (effect  "None.")
	   (value   "A resolution proof, that has as conclusion the formula of the open-node,"
		    "the assumptions are the supports of the open-node and as as upper-object"
		    "and the theory it gets and the environment from the pds."))
  (let* ((pds-name (keim~name pds))
	 (string-name (if (stringp pds-name)
			  pds-name
			(string pds-name)))
	 (supports (pds~node-supports open-node))
	 (non-schematic-supports (remove-if #'(lambda (node)
						(term~schema-p (node~formula node)))
					    supports))
	 (schematic-supports (remove-if-not #'(lambda (node)
						(term~schema-p (node~formula node)))
					    supports))
	 (new-res-proof (res~proof-create
                         (format nil "RES-PROOF-TO-PDS-~A" string-name) 
                         pds
                         (prob~theory pds)
                         (make-instance 'termix+named-term
                                        :term (keim~copy (node~formula open-node) :downto '(data+primitive))
                                        :name (keim~name open-node))
                         (mapcar #'(lambda (assumption)
                                     (make-instance 'termix+named-term
                                                    :term (keim~copy (node~formula assumption) :downto '(data+primitive))
                                                    :name (keim~name assumption)))
                                 non-schematic-supports
                                 ))))
    
    (when schematic-supports
      (omega~message "The following nodes are schematic and thus ignored for the call of the ATP: ~A. You may should first instantiate them!" schematic-supports))
    
    new-res-proof))


(defun atptop~resolution-proof-create-from-nodes (conclusion-node assumption-nodes pds)
  (declare (edited  "25-MAR-1998")
	   (authors Ameier)
	   (input   "A conclusion-node and a list of assumption-nodes and the current-pds.")
	   (effect  "None.")
	   (value   "A new initial resolutionproof."))
  (let* ((pds-name (keim~name pds))
	 (string-name (if (stringp pds-name)
			  pds-name
			(string pds-name)))
	 (non-schematic-assumptions (remove-if #'(lambda (node)
						   (term~schema-p (node~formula node)))
					       assumption-nodes))
	 (schematic-assumptions (remove-if-not #'(lambda (node)
						   (term~schema-p (node~formula node)))
					       assumption-nodes))
	 (new-res-proof (res~proof-create
			 (format nil "RES-PROOF-TO-PDS-~A" string-name) 
			 pds
			 (prob~theory pds)
			 (make-instance 'termix+named-term
					:term (keim~copy (if (pdsn~schematic-p conclusion-node)
							     (pdsn~current-formula conclusion-node)
							   (node~formula conclusion-node))
							 :downto '(data+primitive))
					:name (keim~name conclusion-node))
			 (mapcar #'(lambda (assumption)
				     (make-instance 'termix+named-term
						    :term (keim~copy  (if (pdsn~schematic-p assumption)
									  (pdsn~current-formula assumption)
									(node~formula assumption))
								      :downto '(data+primitive))
						    :name (keim~name assumption)))
				 non-schematic-assumptions))))
    
    (when schematic-assumptions
      (omega~message "The following nodes are schematic and thus ignored for the call of the ATP: ~A. You may should first instantiate them!" schematic-assumptions))
    
    new-res-proof))

(defun atptop~filter-resolution-proof (res-proof)
  (declare (edited  "02-SEP-1998")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "Removes all clauses from the list of clauses that are not pure equality clauses and contain"
		    "only one literal. Removes also all delta-pairs of removed clauses.")
	   (value   "Undefined."))
  (setf (res~proof-clauses res-proof)
	(remove-if-not #'(lambda (clause)
			   (and (= (length (cl~literals clause)) 1)
				(atptop~equation-p clause)))
		       (res~proof-clauses res-proof)))
  
  (let* ((clauses (res~proof-clauses res-proof))
	 (delta-relation (res~proof-delta-relation res-proof)))
    (setf (delta~relation-pairs delta-relation)
	  (remove-if-not #'(lambda (pair)
			     (find (delta~delta-clause pair) clauses :test 'eq))
			 (delta~relation-pairs delta-relation)))))

#| ---------------------------------------------------- REPLACE REFLEX ----------------------------------------------------------- |#

(defun atptop~reflex-clause-of-type (type equality-item)
  (declare (edited  "22-SEP-1998")
	   (authors Ameier)
	   (input   "A type and the current (polymorph) equality item.")
	   (effect  "None.")
	   (value   "A relfex clause with a variable x of this type."))
  (let* ((new-variable (term~variable-create (gensym "X-") (data~copy type :downto '(data+primitive))))
	 (equation (term~appl-create equality-item (list new-variable new-variable)))
	 (literal (lit~literal-create equation 't)))
    (cl~create (list literal)
	       :justification (res~reflex-create))))

#| -------------------------------------------------- Auxiliaries ---------------------------------------------------------------- |#

;;(defun atptop~equation-p (term)
;;  (declare (edited  "22-AUG-1997")
;;	   (authors Ameier)
;;	   (input   "A term.")
;;	   (effect  "None.")
;;	   (value   "T, iff TERM is an equation."))
;;  (cond ((typep term 'lit+literal)
;;	 (atptop~equation-p (lit~atom term)))
;;	(t
;;	 (and (term~appl-p term)
;;	      (string-equal (keim~name (data~appl-function term)) "=")))))

(defgeneric atptop~equation-p (term)
  (declare (edited  "22-AUG-1997")
	   (authors Ameier)
	   (input   "A term.")
	   (effect  "None.")
	   (value   "T, iff TERM is an equation."))
  (:method ((clause cl+clause))
	   (every #'atptop~equation-p (cl~literals clause)))
  (:method ((literal lit+literal))
	   (atptop~equation-p (lit~atom literal)))
  (:method ((term term+term))
	   (and (term~appl-p term)
		(string-equal (keim~name (data~appl-function term)) "="))))

(defun atptop~remove-list (list-to-remove list-from-remove &key (test 'eq))
  (declare (edited  "25-MAR-1996")
	   (authors Ameier)
	   (input   "A list1 of items to remove from a list2 of items and a keyword"
		    "test (default eq) for the check-function.")
	   (effect  "None.")
	   (value   "The list of items from list2 after all items of list1 are removed." ))
  (if list-to-remove
      (let ((head (first list-to-remove))
	    (tail (rest list-to-remove)))
	(atptop~remove-list tail (remove head list-from-remove :test test) :test test))
    list-from-remove))

(defun atptop~read-problem (file)
  (declare (edited  "22-FEB-1996")
	   (authors Ameier)
	   (input   "A file and optional an environment.")
	   (effect  "None.")
	   (value   "The file is tried to read as problem."
		    "This problem is returned."))
  (omega~message "READING PROBLEM ...")
  (with-open-file (stream file :direction :input
			  :if-does-not-exist :error)
		  (let* ((problem-list (read stream)))
		    (post~read-object problem-list (env~create) nil))))

(defun atptop~construct-and-add-problem! (res-proof pds)
  (declare (edited  "03-SEP-1997")
	   (authors Ameier)
	   (input   "A resolution proof and a pds that is constructed bytranslating the resolution-proof.")
	   (effect  "A new problem is created according to the conclusion and the assumption"
		    "of the resolution proof and is inserted into the problem slot of the pds.")
	   (value   "Undefined."))
  (let* ((problem (prob~create (intern (format nil "~A-PROBLEM-~A" (keim~name res-proof) (incf atptop*new-problem-counter) (find-package :keim)))
			       (prob~proof-theory pds)
			       (env~create (list (pds~environment pds)))
			       (mapcar #'(lambda (assumption)
					   (node~create (keim~name assumption)
							(keim~copy (termix~term assumption) :downto '(data+primitive))
							(just~create (infer~find-method 'hyp) nil)))
				       (res~proof-assumptions res-proof))
			       (node~create (keim~name (res~proof-conclusion res-proof))
					    (keim~copy (termix~term (res~proof-conclusion res-proof)) :downto '(data+primitive))
					    (just~create (infer~find-method 'open) nil))
			       (list res-proof pds))))
    (setf (prob~proof-problem pds) problem)
    (setf (prob~proof-problem res-proof) problem)
    
    (setf (gethash (keim::prob=read-string (keim~name problem))  keim::prob*problem-hash-table)
	  problem)))




#| --------------------------------------------- Insert sub proofs ----------------------------------------------------------- |#


(defun atptop~insert-sub-proof-in-original-proof! (original-proof
						   sub-proof
						   node-matching-list)
  (declare (edited  "27-NOV-1996")
	   (authors Ameier)
	   (input   "A ND-proof with a planned-node, called original-proof. An ND-proof called"
		    "sub-proof, that last node is the planned node of the original-proof and"
		    "that hypothesis-nodes are justified nodes of the original proof (not eq !!)."
		    "A list of pairs (l1 l2), with l1 is a Hypothesis node of the sub-proof or the"
		    "last node and l2 is the according justified node of the original proof or"
		    "the planned node. The first pair in the lists has to be the pair of the last-node"
		    "of the sub-proof and the node justified by an atp.")
	   (effect  "All nodes of the sub-proof except the last-node and the hypothesis nodes"
		    "are added to the original-proof. But in their justifications and hypothesis-nodes"
		    "the hypothesis-nodes of the sub-proof are replaced by the according nodes"
		    "of the original-proof. To the atp justification the new justification is added."
		    "Each new node in the original-proof-plan gets as reason the own-reason of the"
		    "node justified by an atp.")
	   (value   "The original proof."))
  (let* ((conc-pair (first node-matching-list))
	 ;; (original-conclusion-node (second conc-pair))
	 (sub-last-node (first conc-pair))
	 (hyps-sub-nodes (mapcar #'first (rest node-matching-list)))
	 (sub-pdsnodes (prob~proof-steps sub-proof))
	 (new-pdsnodes (atptop~remove-list (cons sub-last-node hyps-sub-nodes) sub-pdsnodes)))

    ;; replace in the justs and the hyps of the new nodes (all nodes of the sub-proof except the hyp-nodes) the
    ;; hyp-nodes of the sub-proof by the according nodes from the original proof
    (mapcar #'(lambda (pdsnode)
		(atptop=replace-nodes-in-hyps-and-justs! pdsnode node-matching-list))
	    (cons sub-last-node new-pdsnodes))
    
    ;; insert the new nodes in the original proof
    (mapcar #'(lambda (new-pdsnode)
		(pds~only-insert-node! new-pdsnode original-proof))
	    new-pdsnodes)  
    original-proof))


(defun atptop=replace-nodes-in-hyps-and-justs! (pdsnode node-matching-list)
  (declare (edited  "27-NOV-1996")
	   (authors Ameier)
	   (input   "A pdsnode, and a list of pairs (l1 l2), where l1 and l2 are pdsnodes"
		    "(see input of function atp=insert-sub-proof-in-original-proof!).")
	   (effect  "Every occurrence of a l1 node in the justification"
		    "of the input node is replaced by the according l2 node and"
		    "every occurrence of a l1 node in the hyps of the input node is"
		    "replaced by the hyps of l1.")
	   (value   "The pdsnode."))
  (let* ((hyp-nodes (pdsn~hyps pdsnode))
	 (just (node~justification pdsnode))
	 (premises (just~premises just))
	 (node-hyps-matching-list (mapcar #'(lambda (pair)
					      (list (first pair) (pdsn~hyps (second pair))))
					  node-matching-list)))
    (setf (just~premises just) (atptop=replace-items premises node-matching-list))
    (setf (pdsn~hyps pdsnode) (remove-duplicates 
			       (atptop=replace-items hyp-nodes node-hyps-matching-list :list-as-items 't)))
    pdsnode
    
    ;;   a special handling for asserion justification should'nt be necessary, because the applied assertion is stored
    ;;   as a symbol in the method, and since the names does not change, the symbol also have to bee changed
    
    ))

(defun atptop=replace-items (liste item-matching-list &key (test 'eq) (list-as-items nil))
  (declare (edited  "27-NOV-1996")
	   (authors Ameier)
	   (input   "A list and a list of pairs (i1 i2).")
	   (effect  "None.")
	   (value   "A list consisting of the elements of the input list, but"
		    "each element e that is equal to an i1 of the matching-list is"
		    "replaced by the according i2 element."))
  (do* ((rest-list liste (rest rest-list))
	(back-list nil))
      ((null rest-list) back-list)
    (let* ((head (first rest-list))
	   (member-check (member head item-matching-list :test #'(lambda (item1 item2)
								   (apply test (list item1 (first item2)))))))
      (if member-check
	  (if list-as-items
	      (setq back-list (append back-list (second (first member-check))))
	    (setq back-list (append back-list (rest (first member-check)))))
	(setq back-list (append back-list (list head)))))))




#| ------------------------------------------------ Handling Label Counter -------------------------------------------------------- |#

(defun atptop~compute-label-counter (proof-plan)
  (declare (edited  "03-DEC-1996")
	   (authors Ameier)
	   (input   "A proof-plan.")
	   (effect  "None.")
	   (value   "Computes the highest number of all lines with label Lnumber."))
  (let* ((lines (prob~proof-steps proof-plan))
	 (labels (mapcar #'keim~name lines))
	 (label-strings (mapcar #'(lambda (name)
				    (if (stringp name)
					name
				      (string name)))
				labels))
	 (labels-starting-with-L (remove-if-not #'(lambda (label-string)
						    (equal (char label-string 0) #\L))
						label-strings))
	 (labels-without-L (mapcar #'atptop~cut-first-char labels-starting-with-L))
	 (numbers (apply 'append (mapcar #'(lambda (string)
					     (let* ((number (atptop~parse-number string)))
					       (if number
						   (list number)
						 nil)))
					 labels-without-L))))
    (if numbers
	(eval `(max ,@numbers))
      0)))

#| ------------------------------------------------------ Parse Utilities ---------------------------------------------------------- |#

(defun atptop~cut-first-char (string)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "A new string consisting of the old string without the first char."))
  (do ((i 1 (+ i 1))
       (new-string ""))
      ((>= i (length string)) new-string)
    (setq new-string (format nil "~A~A" new-string (char string i)))))

(defun atptop~cut-x-first-chars (string x)
  (declare (edited  "04-NOV-1999")
	   (authors Ameier)
	   (input   "A string and a number.")
	   (effect  "None.")
	   (value   "A new string consisting of the old string wothout the first x chars."))
  (do ((i x (+ i 1))
       (new-string ""))
      ((>= i (length string)) new-string)
    (setq new-string (format nil "~A~A" new-string (char string i)))))

(defun atptop~cut-last-char (string)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "A new string consisting of the old string without the last char."))
  (do ((i 0 (+ i 1))
       (new-string ""))
      ((>= i (- (length string) 1)) new-string)
    (setq new-string (format nil "~A~A" new-string (char string i)))))

(defun atptop~cut-x-last-chars (string x)
  (declare (edited  "04-NOV-1999")
	   (authors Ameier)
	   (input   "A string and a number.")
	   (effect  "None.")
	   (value   "A new string consisting of the old string wothout the last x chars."))
   (do ((i 0 (+ i 1))
       (new-string ""))
      ((>= i (- (length string) x)) new-string)
    (setq new-string (format nil "~A~A" new-string (char string i)))))

(defun atptop~number-of-char-in-string (char string)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A char and a string.")
	   (effect  "None.")
	   (value   "The number, how often the char appears in string."))
  (do ((i 0 (+ i 1))
       (number 0))
      ((>= i (length string)) number)
    (if (equal (char string i) char)
	(setq number (+ number 1 )))))


(defun atptop~filter-chars (string &key (ignore-char-list nil))
  (declare (edited  "31-MAY-2000")
	   (authors Ameier)
	   (input   "A string and as keyword a list of chars.")
	   (effect  "None.")
	   (value   "A string in which all the occureneces of chars in the ignore-char-list are removed."))
  (do ((i 0 (+ i 1))
       (back-string ""))
      ((>= i (length string))
       back-string)
    (let* ((current-char (char string i)))
      (when (null (find current-char ignore-char-list))
	(setf back-string (format nil "~A~A" back-string current-char))))))

(defun atptop~get-next-word (string break-char &key (handle-break-char nil) (ignore-char-list nil))
  (declare (edited  "15-MAY-1996")
	   (authors Ameier)
	   (input   "A string (consisting of 'words' to divide) ,a break-char, that signs"
		    "who a word ends. The keywords handle-break-char (default nil) that"
		    "signs what should be done with the break-char itself, if pre it is"
		    "added behind to the extract prefix-word, if rest it is added front"
		    "to the rest-string, otherwise it is ignored."
		    "And the keyword ignore-char-list (default nil) to sign chars who"
		    "should be deleted between division."
		    "if beak-char is member ignore-char-list it is deleted everycase, egal"
		    "what standing in handle-break-char.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: The extracted prefix word-string."
		    "Second: The rest-string."))
  (do ((i 0 (+ i 1))
       (break-flag nil)
       (pre-string "")
       (rest-string ""))
      ((>= i (length string)) (values pre-string rest-string))
    (let ((current-char (char string i)))
      (if break-flag
	  (if (not (member current-char ignore-char-list))
	      (setq rest-string (format nil "~A~A" rest-string current-char)))
	(if (equal current-char break-char)
	    (progn
	      (setq break-flag 't)
	      (if (not (member current-char ignore-char-list))
		  (cond ((equal handle-break-char 'pre)
			 (setq pre-string (format nil "~A~A" pre-string current-char)))
			((equal handle-break-char 'rest)
			 (setq rest-string (format nil "~A~A" rest-string current-char))))))
	  (if (not (member current-char ignore-char-list))
	      (setq pre-string (format nil "~A~A" pre-string current-char))))))))


(defun atptop~divide-string (string break-char &key (handle-break-char nil) (ignore-char-list nil))
  (declare (edited  "15-MAY-1996")
	   (authors Ameier)
	   (input   "A string ,consisting of several 'words', and a char to sign"
		    "who the 'words' are separated. And keywords handle-break-char"
		    "ignore-char-list (look atptop~get-next-word).")
	   (effect  "None.")
	   (value   "A list of the divided string."
		    "Example: (atptop~divide-string "DU BLOEDES ARSCH." #\space :ignore-char-list '(#\.))"
		    "   ("DU" "BLOEDES" "ARSCH") " ))
  (if (string= "" string)
      nil
    (do* ((i 0 (+ i 1))
	  (current-string "")
	  (back-list nil))
	((>= i (length string)) (append back-list (list current-string)))
      (let* ((current-char (char string i)))
	(cond ((find current-char ignore-char-list)
	       (when (equal current-char break-char)
		 (setq back-list (append back-list (list current-string)))
		 (setq current-string "")))
	      (t
	       (if (equal current-char break-char)
		   (cond ((equal handle-break-char 'pre)
			  (setq back-list (append back-list (list (format nil "~A~A" current-string current-char))))
			  (setq current-string ""))
			 ((equal handle-break-char 'rest)
			  (setq back-list (append back-list (list current-string)))
			  (setq current-string (format nil "~A" current-char)))
			 (t
			  (setq back-list (append back-list (list current-string)))
			  (setq current-string "")))
		 (setq current-string (format nil "~A~A" current-string current-char)))))))))


;; EXTREM LANGSAM !!!!!!!
;; (defun atptop~divide-string (string break-char &key (handle-break-char 't) (ignore-char-list nil))
;;  (declare (edited  "15-MAY-1996")
;;	   (authors Ameier)
;;	   (input   "A string ,consisting of several 'words', and a char to sign"
;;		    "who the 'words' are separated. And keywords handle-break-char"
;;		    "ignore-char-list (look atptop~get-next-word).")
;;	   (effect  "None.")
;;	   (value   "A list of the divided string."
;;		    "Example: (atptop~divide-string "DU BLOEDES ARSCH." #\space :ignore-char-list '(#\.))"
;;		    "   ("DU" "BLOEDES" "ARSCH") " ))
;; (if (string= "" string)
;;      nil
;;   (multiple-value-bind
;;	(new-word rest-string)
;;	(atptop~get-next-word string break-char :handle-break-char handle-break-char :ignore-char-list ignore-char-list)
;;     (cons new-word (atptop~divide-string rest-string break-char :handle-break-char handle-break-char))
;;      ;; ignore-char-list nil cause you only need one time the ignoration of this chars
;;      )))

(defun atptop~divide-string-by-lists (in-string break-char-list ignore-char-list)
  (declare (edited  "03-SEP-1997")
	   (authors Ameier)
	   (input   "A string and two lists of chars.")
	   (effect  "None.")
	   (value   "A list of strings. Remark that it is possible that empty strings are contained in the list"
		    "since every string between two break-chars is returned (end and finish can also be seen as"
		    "break chars). So if two break-chars are directly following this causes an empty string.")) 
  (do* ((i 0 (+ i 1))
	(back-list nil)
	(current-string ""))
      ((= i (length in-string))
       ;; back-list)
       (append back-list (list current-string)))
    (let* ((current-char (char in-string i)))
      (cond ((find current-char break-char-list)
	     (setq back-list (append back-list (list current-string (format nil "~A" current-char))))
	     (setq current-string ""))
	    ((find current-char ignore-char-list)
	     nil)
	    (t
	     (setq current-string (format nil "~A~A" current-string current-char)))))))

	    
(defun atptop~char2number (char)
  (declare (edited  "17-MAY-1996")
	   (authors Ameier)
	   (input   "A char and as keyword a list of additional characters.")
	   (effect  "None.")
	   (value   "If char is a number-char the according number is returned."
		    "otherwise nil."))
  (cond ((equal char #\0) 0)
	((equal char #\1) 1)
	((equal char #\2) 2)
	((equal char #\3) 3)
	((equal char #\4) 4)
	((equal char #\5) 5)
	((equal char #\6) 6)
	((equal char #\7) 7)
	((equal char #\8) 8)
	((equal char #\9) 9)
	(t nil))) 

(defun atptop~parse-number (number-string)
  (declare (edited  "28-MAY-1996")
	   (authors Ameier)
	   (input   "A string and as keyword a list of additional characters.")
	   (effect  "None.")
	   (value   "If string is a number-string, the string is parsed and returned is"
		    "the according number, otherwise nil."))
  (let ((laenge (- (length number-string) 1)))
    (do ((i 0 (+ i 1))
	 (exp 1 (* exp 10))
	 (number 0)
	 (flag t))
	((or (null flag) (= i (+ laenge 1))) (if (null flag) nil number))
      (let ((new-part (atptop~char2number (char number-string (- laenge i)))))
	(if new-part
	    (setq number (+ number (* exp new-part)))
	  (setq flag nil))))))

(defun atptop~parse-float-number (number-string)
  (declare (edited  "23-NOV-2004" )
	   (authors Vxs)
	   (input   "A string.")
	   (effect  "None.")
	   (value   "If string is a float number-string, the string is parsed and returned is"
		    "the according number, otherwise nil."))
  (flet ((sign (str)
	       (when (and str (not (equal str "")))
		 (let ((fstr (aref str 0)))
		   (if (or (string-equal fstr #\+) (string-equal fstr #\-))
		       (values (subseq str 1) fstr)
		     (values str nil)))))
	 (parse (str)
		(unless (or (not str) (string-equal str "")) (atptop~parse-number str))))
    (let ((exp-parts
	   (remove-if #'(lambda (x) (find x '("e" "E" "") :test #'string-equal))
		      (atptop~divide-string-by-lists number-string '(#\e #\E) nil))))
      (when (or (= (length exp-parts) 2) (= (length exp-parts) 1))
	(multiple-value-bind (exp exp-sign)
	    (sign (cadr exp-parts))
	  (multiple-value-bind (base base-sign)
	      (sign (car exp-parts))
	    (let* ((de-fraction (atptop~divide-string base #\.))
		   (int (apply #'concatenate
			       (cons 'string
				     (atptop~divide-string (car de-fraction) #\,))))
		   (real-int (parse int))
		   (frac (cadr de-fraction))
		   (real-frac (parse frac))
		   (mexp (parse exp))
		   (real-base (cond ((and base-sign real-int real-frac)
				     (format nil "~A~A.~A" base-sign real-int real-frac))
				    ((and base-sign real-int)
				     (format nil "~A~A" base-sign real-int))
				    ((and real-int real-frac)
				     (format nil "~A.~A" real-int real-frac))
				    (real-int real-int)
				    (t nil)))
		   (real-exp (cond ((and exp-sign mexp)
				    (format nil "~A~A" exp-sign mexp))
				   (mexp mexp)
				   (t nil))))
	      (cond ((and real-base real-exp)
		     (read-from-string (format nil "~Ae~A" real-base real-exp)))
		    (real-base
		     (read-from-string (format nil "~A" real-base)))
		    (t nil)))))))))	

(defun atptop~parse-only-numbers (string &key (begin-list nil) (float nil))
  (declare (edited  "23-NOV-2004" "23-MAR-1998")
	   (authors Vxs Ameier)
	   (input "A string and as keyword a list of chars and a flag whether"
		  "the number should be parsed as float (including dots, kommata and e notation).")
	   (effect  "None.")
	   (value   "A list of all numbers in the string. If begin-list is not nil"
		    "only numbers after char in the begin-list are parsed."
		    "Examples:/"Roboter 234 hat 120 Eier in 239 Sekunden gegessen/""
		    "         -> (234 120 239)."))
  (let* ((laenge (length string))
	 (special-list (when float '( #\. #\, #\e #\E #\- #\+)))
	 (number-list (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
	 (numbers (append  number-list special-list)))
    (do* ((i 0 (1+ i))
	  (number-strings nil)
	  (current-string (if (null begin-list)
			      ""
			    nil)))
	((= i laenge)
	 (if float
	     (remove-if #'null
			(mapcar #'atptop~parse-float-number
				(if (and current-string (not (string= current-string "")))
				    (append number-strings (list current-string))
				  number-strings)))
	   (mapcar #'atptop~parse-number (if (and current-string (not (string= current-string "")))
					     (append number-strings (list current-string))
					   number-strings))))
      (let* ((current-char (char string i)))
	(cond ((find current-char begin-list)
	       (setq current-string ""))
	      ((find current-char numbers)
	       (when current-string
		 (setq current-string (format nil "~A~A" current-string current-char))))
	      (t
	       (when (and current-string (not (string= current-string "")))
		 (progn
		   (setq number-strings (append number-strings (list current-string)))
		   (setq current-string (if (null begin-list)
					    ""
					  nil))))))))))


(defun atptop~string-is-prefix-of-string-p (prefix-string string)
  (declare (edited  "24-MAY-1996")
	   (authors Ameier)
	   (input   "Two strings.")
	   (effect  "None.")
	   (value   "If first string is prefix of second-string, the suffix of second"
		    "is returned, otherwise nil."
		    "F.E.  \"DU \" \"DU ARSCH\" -> \"ARSCH\"."))
  (do* ((i 0 (+ i 1))
	(flag t))
      ((or (null flag)
	   (>= i (length prefix-string))
	   (>= i (length string)))
       (cond ((null flag)
	      nil)
	     ((>= i (length prefix-string))
	      (do* ((j i (+ j 1))
		    (back-string ""))
		  ((>= j (length string)) back-string)
		(setq back-string (format nil "~A~A" back-string (char string j)))))
	     (t
	      nil)))
    (when (not (equal (char string i) (char prefix-string i)))
      (setq flag nil))))

#| ------------------------------------------------- equal-till-renaming ---------------------------------------------------------- |#

(defun atptop~clauses-equal-till-renaming-and-ordering-p (clause1 clause2 &key (flip nil))
  (declare (edited  "27-AUG-1998")
	   (authors Ameier)
	   (input   "Two clauses and as keyword flip a boolean to sign whether the equality check should"
		    "also be done modulo flippings of equality literals.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two clauses are equal till renaming and ordering, that means"
		    "       there exists a renaming, that if applied on a clause will make the"
		    "       literal-sets of the clauses equal, only the ordering of the literals in the"
		    "       clause can differ, otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make clause1 equal"
		    "        clause2."
		    "Third: If first is nil, nil too. Otherwise a list of position pairs"
		    "       that are determinig which which literal at a position in clause1"
		    "       corresponds to which literal at a position in clause2."
		    "Fourth: If first is nil or flip is nil also nil, otherwise a list of positions"
		    "        of the literals in the second clause that are flipped in comparison"
		    "        with literals in the first clause."))
  (if (null (= (length (cl~literals clause1)) (length (cl~literals clause2))))
      nil
    (multiple-value-bind
	(renamed-clause2 renaming)
	(res~separate-clauses clause1 clause2)
      (declare (ignore renaming))

      (multiple-value-bind
	  (flag var-matching pos-matching flip-list)
	  (atptop=literals-equal-till-r+o+f (cl~literals clause1)
					    (cl~literals renamed-clause2)
					    (cl~literals renamed-clause2)
					    nil nil 0 :flip flip)
	(values flag
		(if flag
		    (let* ((reverse-renaming (subst~create (subst~codomain renaming) (subst~domain renaming))))
		      (mapcar #'(lambda (var-pair)
				  (list (first var-pair)
					(subst~apply reverse-renaming (second var-pair)))) 
			      var-matching))
		  nil)
		pos-matching
		flip-list
		)))))

(defun atptop=literals-equal-till-r+o+f (rest-literals1 rest-literals2 all-literals2 curr-var-matching curr-pos-matching curr-pos1
							&key (flip nil))
  (if (null rest-literals1)
      (values 't curr-var-matching curr-pos-matching)
    
    (let* ((head-literal1 (first rest-literals1))
	   (head-position (pos~list-position (list curr-pos1)))
	   (possible-equal-literals2 (remove-if-not #'(lambda (lit2)
							(atptop=literals-equal-till-renaming-p head-literal1 lit2))
						    rest-literals2))
	   (possible-equal-literals2-flip (if (and flip (atptop~equation-p head-literal1))
					      (remove-if-not #'(lambda (lit2)
								 (atptop=literals-equal-till-r+f-p head-literal1 lit2))
							     rest-literals2)
					    nil)))
      (do* ((rest-pos-eq-lits2 possible-equal-literals2)
	    (rest-pos-eq-lits2-flip possible-equal-literals2-flip)
	    (flag nil)
	    (var-matching nil)
	    (pos-matching nil)
	    (flip-list nil)
	    )
	  ((or flag (and (null rest-pos-eq-lits2)
			 (null rest-pos-eq-lits2-flip)))
	   (if flag
	       (values 't var-matching pos-matching flip-list)
	     (values nil nil nil nil)))
	(let* ((next-literal2 (if rest-pos-eq-lits2
				 (first rest-pos-eq-lits2)
			       (first rest-pos-eq-lits2-flip)))
	       (flip-lit-p (if rest-pos-eq-lits2
			       nil
			     't)))
  
	  (multiple-value-bind
	      (literals-equal-flag new-var-matchings) 
	      (if flip-lit-p
		  (atptop=literals-equal-till-r+f-p head-literal1 next-literal2)
		(atptop=literals-equal-till-renaming-p head-literal1 next-literal2))
	    (multiple-value-bind
		(matching-union-flag union-var-matching-list)
		(atptop=matching-union curr-var-matching new-var-matchings)
	      (when matching-union-flag
		(multiple-value-bind
		    (new-flag new-var-matching new-pos-matching new-flip-list)
		    (atptop=literals-equal-till-r+o+f (rest rest-literals1)
						      (remove next-literal2 rest-literals2)
						      all-literals2
						      union-var-matching-list
						      (append curr-pos-matching
							      (list (list head-position (first (data~substruct-positions next-literal2
															 all-literals2
															 :test #'eq)))))
						      (+ curr-pos1 1)
						      :flip flip)
		  (when new-flag
		    (setq flag new-flag)
		    (setq var-matching new-var-matching)
		    (setq pos-matching new-pos-matching)
		    (setq flip-list new-flip-list)

		    (when flip-lit-p
		      (setq flip-list (cons (first (data~substruct-positions next-literal2
									     all-literals2
									     :test #'eq)) flip-list)))
		    )))))

	  (if flip-lit-p
	      (setq rest-pos-eq-lits2-flip (rest rest-pos-eq-lits2-flip))
	    (setq rest-pos-eq-lits2 (rest rest-pos-eq-lits2))))))))

		  

(defun atptop=literals-equal-till-r+f-p (lit1 lit2)
  (declare (edited  "28-AUG-1998")
	   (authors Ameier)
	   (input   "Two literals.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: t if both literals are equations and one literal is equal to the other"
		    "       module renaming and flippong, nil otherwise."
		    "Second: nil if first is nil, the variable matching of the renaming if first is true."))
  (if (null (and (atptop~equation-p lit1) (atptop~equation-p lit2)))
      (values nil nil)
    (let* ((flip-lit2 (lit~literal-create (term~appl-create (data~appl-function (lit~atom lit2))
							    (reverse (data~appl-arguments (lit~atom lit2))))
					  (lit~positive-p lit2))))
      (multiple-value-bind
	  (flag matching)
	  (atptop=literals-equal-till-renaming-p lit1 flip-lit2)
	(values flag
		matching)))))
						  
(defun atptop~clauses-equal-till-renaming-p (clause1 clause2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two clauses.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two clauses are equal till renaming, that means"
		    "       there exists a renaming, that if applied on a clause will make the"
		    "       clauses equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make clause1 equal"
		    "        clause2."))
  (if (= (length (cl~literals clause1)) (length (cl~literals clause2)))
      (multiple-value-bind
	  (renamed-clause2 renaming)
	  (res~separate-clauses clause1 clause2)
	(declare (ignore renaming))
	(do ((rest-literals1 (cl~literals clause1) (rest rest-literals1))
	     (rest-literals2 (cl~literals renamed-clause2) (rest rest-literals2))
	     (flag 't)
	     (var-matching-list nil))
	    ((or (null flag) (null rest-literals1))
	     (values flag (if (null flag)
			      nil
			    (let* ((reverse-renaming (subst~create (subst~codomain renaming) (subst~domain renaming))))
			      (mapcar #'(lambda (var-pair)
					  (list (first var-pair)
						(subst~apply reverse-renaming (second var-pair)))) 
				      var-matching-list)))))
	  (multiple-value-bind
	      (literals-equal-flag new-var-matchings) 
	      (atptop=literals-equal-till-renaming-p (first rest-literals1) (first rest-literals2))
	    (if (null literals-equal-flag)
		(setq flag nil)
	      (multiple-value-bind
		  (matching-union-flag union-var-matching-list)
		  (atptop=matching-union var-matching-list new-var-matchings)
		(if (null matching-union-flag)
		    (setq flag nil)
		  (setq var-matching-list union-var-matching-list)))))))
    (values nil nil)))

;; matching-list consists of pairs var1/var2 from vars in clause1 and clause2 at the same position

(defun atptop=matching-union (matching1 matching2)
  (do ((rest-add-matching matching2 (rest rest-add-matching))
       (union-matching matching1)
       (flag 't))
      ((or (null flag) (null rest-add-matching)) (values flag union-matching))
    (let* ((current-pair (first rest-add-matching))
	   (conter-pair-of-var1 (first (member (first current-pair) matching1 :test #'(lambda (var pair)
											(keim~equal var (first pair))))))
	   (conter-pair-of-var2 (first (member (second current-pair) matching1 :test #'(lambda (var pair)
											 (keim~equal var (second pair)))))))
      (if (or conter-pair-of-var1 conter-pair-of-var2)
	  (if (not (and (keim~equal current-pair conter-pair-of-var1) (keim~equal current-pair conter-pair-of-var2)))
	      (setq flag nil))
	(setq union-matching (cons current-pair union-matching))))))

(defun atptop=literals-equal-till-renaming-p (literal1 literal2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two literals.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two literals are equal till renaming, that means"
		    "       there exists a renaming, that if applied on one literal will make the"
		    "       literals equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make literal1 equal"
		    "        literal2."))
  (if (equal (lit~positive-p literal1) (lit~positive-p literal2))
      (atptop=terms-equal-till-renaming-p (lit~atom literal1) (lit~atom literal2))
    (values nil nil)))

(defgeneric atptop=terms-equal-till-renaming-p (term1 term2)
  (declare (edited  "17-SEP-1996")
	   (authors Ameier)
	   (input   "Two terms.")
	   (effect  "None.")
	   (value   "Multiple-value-bind:"
		    "First: T if the two terms are equal till renaming, that means"
		    "       there exists a renaming, that if applied on a term will make the"
		    "       terms equal. otherwise nil."
		    "Second: If first is nil,nil too. Otherwise a list of variable pairs"
		    "        that are representing are renaming, to make term1 equal"
		    "        term2."))
  (:method ((term1 term+appl) (term2 term+appl))
	   (if (keim~equal (data~appl-function term1) (data~appl-function term2))
	       (do ((rest-terms1 (data~appl-arguments term1) (rest rest-terms1))
		    (rest-terms2 (data~appl-arguments term2) (rest rest-terms2))
		    (flag 't)
		    (var-matching-list nil))
		   ((or (null flag) (null rest-terms1)) (values flag var-matching-list))
		 (multiple-value-bind
		     (terms-equal-flag new-var-matchings) 
		     (atptop=terms-equal-till-renaming-p (first rest-terms1) (first rest-terms2))
		   (if (null terms-equal-flag)
		       (setq flag nil)
		     (multiple-value-bind
			 (matching-union-flag union-var-matching-list)
			 (atptop=matching-union var-matching-list new-var-matchings)
		       (if (null matching-union-flag)
			   (setq flag nil)
			 (setq var-matching-list union-var-matching-list))))))
	     (values nil nil)))
  (:method ((term1 term+variable) (term2 term+variable))
	   (values 't (list (list term1 term2))))
  (:method ((term1 term+constant) (term2 term+constant))
	   (if (keim~equal term1 term2)
	       (values 't nil)
	     (values nil nil)))
  (:method (term1 term2)
	   (declare (ignore term1 term2))
	   (values nil nil)))

#| ------------------------------------------------- call process with time resource ------------------------------------------- |#

(defun atptop=char-to-int (c)
  (case c
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)))

(defun atptop=string-to-int (str)
  (reduce #'(lambda (a b) (+ (* 10 a) b))
	  (map 'list #'atptop=char-to-int str)))

(defun atptop=extract-pid-from-string (str)
  (let ((pos (position #\] str :test #'char=)))
    (when (and pos (numberp (read-from-string (subseq str (+ 2 pos) (+ 3 pos)))))
	(atptop=string-to-int (subseq str (+ 2 pos)))))
  )

(defun atptop~get-pid-from-stream (streami)
  (declare (edited  "26-MAY-1998")
	   (authors Ameier)
	   (input   "A stream (output-stream from calling atp).")
	   (effect  "Read from the stream.")
	   (value   "The pid of the atp-process."))
  (let* ((line (read-line streami nil '+-*/))
	 (pid nil))
    (omega~message "LINE: ~A" line)
    
    (loop while (not (equal line '+-*/))
	  do
	  
	  (setq pid (atptop=extract-pid-from-string line))
	  (if pid
	      (setq line '+-*/)
	    (setq line (read-line streami nil '+-*/))))
    (unless pid (omega~warn "Konnte PID nicht finden"))
    
    pid))


	 
(defun atptop~call-with-time-ressource (shell-call out-file-name program-name directory resource-in-sec
						   &key (agent-name nil))
  (declare (edited  "29-JAN-1998")
	   (authors ChrisSorge AMEIER)
	   (input   "The command to call in the shell as a string, the name of the out-file (as string),"
		    "the name of the program (as string), the working directory and a"
		    "time resource in seconds.")
	   (effect  "First the outfile is removed, then the command is started in the shell, with all"
		    "things, that this may will cause ...")
	   (value   "T if the out-file is written by the processes themself after they had finished, nil"
		    "if the processes doesn't stopped during the time given by the time resource and had"
		    "to be killed."))
  (mixin~save-and-set-input)
  (when (probe-file out-file-name)
    (sys~call-system (format nil "\\rm ~A" out-file-name)))

  (multiple-value-bind
      (stream-a dummy-a dummy-b) 
      (excl::run-shell-command shell-call
			       :wait nil :output :stream)
    (declare (ignore dummy-a)) ;;  dummy-b))
    
    (let* ((time (get-universal-time))
	   (killing-time (+ resource-in-sec time))
	   (pid (atptop~get-pid-from-stream stream-a))
	   (agent))

      (close stream-a)

      (when agent-name
	(progn
	 (setf agent (agent~get-agent agent-name))
	 (setf (agent~proc-id agent) (append (agent~proc-id agent) (list (list pid
									       directory))))
	 ;; add agent to list with all agents for external
	 ;; provers
	 (setf agent*external-prover-agents (cons agent agent*external-prover-agents))))
	 
      (omega~message "~% Calling ~A process ~A with time resource ~Asec ."
		     program-name
		     pid
		     resource-in-sec)
      
      (omega~message "~% ~A Time Resource in seconds: " program-name)
      
      (let ((proof-found-flag (atptop=back-counter out-file-name killing-time pid directory)))
	(mixin~reset-input)
	
#+(and allegro-version>= (version>= 5 0)) (sys:reap-os-subprocess :wait nil :pid dummy-b)
#+(not (and allegro-version>= (version>= 5 0))) (sys:os-wait :wait nil :pid dummy-b)
	
	proof-found-flag))))

(defun atptop=back-counter (out-file-name killing-time pid directory)
  (declare (edited  "29-JAN-1998")
	   (authors ChrisSorge AMEIER)
	   (input   "The name of the outfile, A killing (universal) time, the pid of"
		    "the started processes and the working directory.")
	   (effect  "Waits for killing time and then kills running process. If the processes"
		    "end themself before killing-time is reached, then nothing happens.")
	   (value   "T if the processes killed themself, nil otherwise."))
  (if (probe-file out-file-name)
      't
    (cond  ((null pid);MP process already finished before we got the pid but without output file
	    nil)
	   ((and (> killing-time (get-universal-time))
		 (> (- killing-time (get-universal-time)) 0))
	    (omega~message " ~Asec" (- killing-time (get-universal-time)))
	    (sleep 1)
	    (atptop=back-counter out-file-name killing-time pid directory))
	   (t
	    (omega~message "~% No Time Resources left. ")
	    (if (null atptop*interactivity-allowed)
		(let* ((pids-of-children (atptop~get-pids-of-children pid directory)))
		  (if (probe-file out-file-name)
		      ;; prueft noch ein letztes mal ob outfile inzwischen da 
		      't
		    (progn (atptop~kill pids-of-children)
			   nil)))
	      (let ((query (omega~query "Quit process [y] or spent a new Time Resource [n]? " (arg~find-argtype 'boolean))))
		(if query
		    (let* ((pids-of-children (atptop~get-pids-of-children pid directory)))
		      (if (probe-file out-file-name)
			  ;; prueft noch ein letztes mal ob outfile inzwischen da 
			  't
			(progn (atptop~kill pids-of-children)
			       nil)))
		  (let ((new-resource (inter~prompt-for-input  (comint~interface comint*current-comint)
								" New Time Resource in seconds: "
								(arg~find-argtype 'integer))))
		    (atptop=back-counter out-file-name
					 (+ new-resource (get-universal-time))
					 pid
					 directory)))))))))


#|MP: same without recursion. helps to get of strange errors with external calls
(defun atptop=back-counter (out-file-name killing-time pid directory)
  (declare (edited  "29-JAN-1998")
	   (authors ChrisSorge AMEIER)
	   (input   "The name of the outfile, A killing (universal) time, the pid of"
		    "the started processes and the working directory.")
	   (effect  "Waits for killing time and then kills running process. If the processes"
		    "end themself before killing-time is reached, then nothing happens.")
	   (value   "T if the processes killed themself, nil otherwise."))
  (loop
   (cond ((probe-file out-file-name) (return 't))
	 ((and (> killing-time (get-universal-time))
	       (> (- killing-time (get-universal-time)) 0))
	  (omega~message " ~Asec" (- killing-time (get-universal-time)))
	  (sleep 1))
	 (t
	  (if (null atptop*interactivity-allowed)
	      (let* ((pids-of-children (atptop~get-pids-of-children pid directory)))
		(if (probe-file out-file-name)
		    ;; prueft noch ein letztes mal ob outfile inzwischen da 
		    (return 't)
		  (progn (atptop~kill pids-of-children)
			 (return nil))))
	    (let ((query (omega~query "Quit process [y] or spent a new Time Resource [n]? " (arg~find-argtype 'boolean))))
	      (if query
		  (let* ((pids-of-children (atptop~get-pids-of-children pid directory)))
		    (if (probe-file out-file-name)
			;; prueft noch ein letztes mal ob outfile inzwischen da 
			(return 't)
		      (progn (atptop~kill pids-of-children)
			     (return nil))))
		(let ((new-resource (inter~prompt-for-input  (comint~interface comint*current-comint)
							     " New Time Resource in seconds: "
							     (arg~find-argtype 'integer))))
		  (setf killing-time (+ new-resource (get-universal-time)))))))))))
|#

(defun atptop=get-new-file-name (directory)
  (declare (edited  "23-MAR-1998")
	   (authors Ameier)
	   (input   "A directory.")
	   (effect  "None.")
	   (value   "A file name: directory/pstreeN.asc, with N is a number, that the file"
		    "is new."))
  (do* ((current-number 0 (incf current-number))
	(current-file-name (format nil "pstree~A.asc" current-number)
			   (format nil "pstree~A.asc" current-number)) 
	(current-file-name-comp (merge-pathnames current-file-name directory)
				(merge-pathnames current-file-name directory)))
      ((null (probe-file current-file-name-comp)) current-file-name-comp)))


(defun atptop~get-pids-of-children (pid directory)
  (declare (edited  "23-MAR-1998")
	   (authors Ameier)
	   (input   "The pid of a process and an existing directory.")
	   (effect  "In the directory is temporary a file with name pstree.asc created.")
	   (value   "Calls the shell-command pstree, writes the output in a file and lookup the"
		    "pids of all children processes of the process with PID. The value is a list"
		    "of pids including the input pid."))
  (let* ((file-name (atptop=get-new-file-name directory))
	 (shell-com (cond ((string= (sys~getenv "HOSTTYPE") "sun4")
			   (format nil "ps -Af >! ~A" file-name))
			  (t
			   (format nil "pstree -p >! ~A" file-name))))
	 (shell-del (format nil "\\rm ~A" file-name)))
    
    ;; schreibt pstree (in linux) bzw. ps mit Unterbeziehungen (unter solaris) in file FILE-NAME
    (sys~call-system shell-com)
    
    (let* ((string-lines
	    ;; liest file FILE-NAME in stream und gibt es als Liste von Strings zurueck, die die einzelnen Zeilen repraesentieren
	    (with-open-file (stream file-name
				    :direction :input)
			    (do* ((eof-flag nil)
				  (lines-list nil))
				(eof-flag lines-list)
			      (let* ((string-line (read-line stream nil 'schnuddelbuddelbauteinhaus)))
				(if (equal string-line 'schnuddelbuddelbauteinhaus)
				    (setq eof-flag 't)
				  (setq lines-list (cons string-line lines-list))))))))
      
      ;; loecht temporary file
      (sys~call-system shell-del)
            
      ;;(excl::run-shell-command shell-del :wait nil :output :stream)
      ;;(sys:reap-os-subprocess)
      
      (cond ((string= (sys~getenv "HOSTTYPE") "sun4")
	     (atptop=parse-pids-sun4 string-lines pid))
	    (t
	     (atptop=parse-pids-linux string-lines pid))))))

(defun atptop=parse-pids-sun4 (string-lines pid)
  (let* ((pid-search-string (format nil "~A" pid))
	 (candidat-string-lines (remove-if-not #'(lambda (line) 
						   (search pid-search-string line))
					       string-lines)))
    (do* ((rest-lines candidat-string-lines (rest rest-lines))
	  (back-numbers (list pid)))
	((null rest-lines) back-numbers)
      (let* ((head-line (first rest-lines))
	     (numbers-of-line (atptop~parse-only-numbers head-line)))
	
	;; erste number ist PID process, zweite number ist PID parent process
	(when (= (second numbers-of-line) pid)
	  (setq back-numbers (append back-numbers (list (first numbers-of-line)))))))))

(defun atptop=parse-pids-linux (string-lines pid)
  ;; sucht String-Zeile, in der '(PID)' substring von string-line ist
  (let* ((pid-search-string (format nil "(~A)" pid))
	 (needed-string-line (first (remove-if-not #'(lambda (line)
						       (search pid-search-string line))
						   string-lines))))
    (atptop=parse-only-numbers-in-brackets needed-string-line)))

(defun atptop=parse-only-numbers-in-brackets (string-line)
  (let* ((laenge (length string-line)))
    (do* ((counter 0 (+ counter 1))
	  (back-numbers ())
	  (para-flag nil)
	  (current-para-string nil))
	((= counter laenge)
	 back-numbers)
      (let* ((current-char (char string-line counter)))
	(cond ((equal #\( current-char)
	       (setf para-flag 't)
	       (setf current-para-string ""))
	      ((equal #\) current-char)
	       (setf para-flag nil)
	       (when (atptop~parse-number current-para-string)
		 (setf back-numbers (append back-numbers (list (atptop~parse-number current-para-string))))))
	      (para-flag
	       (setf current-para-string (format nil "~A~A" current-para-string current-char))))))))


(defmethod atptop~kill ((pids list))
  (declare (edited  "29-JAN-1998")
	   (authors Chris AMEIER)
	   (input   "A list of pid's.")
	   (effect  "Kills the according processes.")
	   (value   "Undefined"))
  (mapcar #'(lambda (pid)
	      (omega~message "~% Killing Process with pid ~A. " pid)
	      (sys~call-system (format nil "kill -9 ~A" pid)))
	  pids))


(defun atptop=get-pids-of-process (process-string)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A string with a process-name.")
	   (effect  "None.")
	   (value   "A list of pids of a running process with this name, nil if not existing."))
  (let* ((call-string (format nil "pidof ~A" process-string)))
    (multiple-value-bind
	(number-stream dummy-a dummy-b)
	(excl::run-shell-command call-string
				 :wait nil :output :stream)
      (declare (ignore dummy-a dummy-b))

#+(and allegro-version>= (version>= 5 0)) (sys:reap-os-subprocess)
#+(not (and allegro-version>= (version>= 5 0))) (sys:os-wait)
      
      (do* ((number-list nil)
	    (eof-flag nil))
	  (eof-flag number-list)
	(let ((next-read (read number-stream nil 'pups)))
	  (if (equal next-read 'pups)
	      (setq eof-flag 't)
	    (setq number-list (cons next-read number-list))))))))



    

	   
#| ----------------------------------------------- READING and Writing strings as files --------------------------------------------- |#

(defun atptop~read-file-as-string (file-name)
  (declare (edited  "30-MAR-1998")
	   (authors Ameier)
	   (input   "A file.")
	   (effect  "None.")
	   (value   "A string, with the contents of the file."))
  (with-open-file (stream file-name
			  :direction :input)
		  (do* ((eof-flag nil)
			(return-string ""))
		      (eof-flag return-string)
		    (let ((string-line (read-line stream nil '+-*/)))
		      (if (equal string-line '+-*/) 
			  (setq eof-flag 't)
			(setq return-string (format nil "~A~A~A"
						    return-string
						    #\Newline
						    string-line)))))))


(defun atptop~print-string-in-file (string file)
  (declare (edited  "26-MAR-1998")
	   (authors Ameier)
	   (input   "A string and a file.")
	   (effect  "Writes the string in the file.")
	   (value   "Undefined."))
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede)
		  (format stream "~A" string)))

#| ------------------------------------------------------ COPY RES-PROOF ------------------------------------------------------------ |#

#| Benoetigt um falls ein F.O. Problem bereits ubersetzt und normalisiert wurde dieses Zeugs einfach zu kopieren und nicht nochmal
   neu zu machen. |#

;; VORSICHT: 1.) NICHT GETESTET

(defun atptop~copy-res-proof (res-proof)
  (declare (edited  "22-APR-1998")
	   (authors Ameier)
	   (input   "A initial resolution proof (only initial clauses).")
	   (effect  "None.")
	   (value   "A copy of the resolution proof."))
  (let* ((pds (res~proof-upper-object res-proof))
	 (res-name (keim~name res-proof))
	 (old-conclusion (res~proof-conclusion res-proof))
	 (new-conclusion (termix~create-named-term (keim~name old-conclusion)
						   (keim~copy (termix~term old-conclusion) :downto '(data+primitive))))
	 (old-assumptions (res~proof-assumptions res-proof))
	 (new-assumptions (mapcar #'(lambda (old-assumption)
				      (termix~create-named-term (keim~name old-assumption)
								(keim~copy (termix~term old-assumption) :downto '(data+primitive))))
				  old-assumptions))

	 (old-clauses (res~proof-initial-clauses res-proof))
	 (new-clauses (mapcar #'(lambda (old-clause)
				  (keim~copy old-clause :downto '(data+constant)))
			      old-clauses))
	 
	 (new-res-proof (res~proof-create
			 (format nil "RES-COPY-OF-~A" res-name)
			 pds
			 (prob~theory pds)
			 new-conclusion
			 new-assumptions))

	 (old-env (res~proof-environment res-proof))
	 (new-env (res~proof-environment new-res-proof))
	 (new-delta-relation (delta~create-relation))
	 (old-formulas (cons old-conclusion old-assumptions))
	 (new-formulas (cons new-conclusion new-assumptions)))
    
    ;; fuegt delta-relation ein
    (mapcar #'(lambda (old-pair)
		(let* ((old-formula (delta~delta-formula old-pair))
		       (old-position-in-formula (delta~delta-position-in-formula old-pair))
		       (old-clause (delta~delta-clause old-pair))
		       (old-position-in-clause (delta~delta-position-in-clause old-pair)))
		  (delta~add-pair! new-delta-relation
				   (data~assoc old-formula old-formulas new-formulas #'eq)
				   (keim~copy old-position-in-formula)
				   (data~assoc old-clause old-clauses new-clauses #'eq)
				   (keim~copy old-position-in-clause))))
	    (delta~relation-pairs (res~proof-delta-relation res-proof)))
    (setf (res~proof-delta-relation new-res-proof) new-delta-relation)

    ;; fuegt initial-clauses ein
    (setf (res~proof-clauses new-res-proof) new-clauses)

    ;; fuegt skolem-functions ein
    (setf (res~proof-skolem-functions new-res-proof) (copy-list (res~proof-skolem-functions res-proof)))

    ;; fuegt neue Elemente (im wesentlichen skolem-functions) in environment ein
    (mapcar #'(lambda (element)
		(env~enter (keim~name element) element new-env))
	    (append (env~class-keys old-env 'type+variable nil)
		    (env~class-keys old-env 'type+constant nil)
		    (env~class-keys old-env 'term+constant nil)
		    (env~class-keys old-env 'term+variable nil)))

    new-res-proof))


(defun atptop~remove-clauses-with-abstractions! (res-proof)
  (declare (edited  "22-APR-1998")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "Takes all clauses of the resolution proof and removes all these clauses that"
		    "contain lambda-abstractions.")
	   (value   "Undefined."))
  (let* ((clauses (res~proof-clauses res-proof))
	 (remaining-clauses (remove-if #'(lambda (clause)
					   (let* ((literals (cl~literals clause))
						  (atoms (mapcar #'lit~atom literals)))
					     (some #'(lambda (term)
						       (data~positions term #'data~abstr-p))
						   atoms)))
				       clauses)))
    (setf (res~proof-clauses res-proof) remaining-clauses)))
						  
