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



(mod~defmod LORD 
            :uses (cl data extdelta keim lit node plco pos r2ntop ref split subst)
            :documentation "Computation of the link order."
            :exports (
                      lord+ordering-pair
                      lord+trail
                      
                      lord~check-unit-before-splitting!
                      lord~create-ordering-pair
                      lord~create-trail
                      ))




#| ------------------------------------------------------- lord+ordering-pair ------------------------------------------------------ |#

(eval-when (load compile eval)
  (defclass lord+ordering-pair (keim+object)
    ((lesser-link :initarg :lesser-link
		  :initform nil
		  :reader lord=get-lesser-link
		  :writer lord=set-lesser-link!
		  :documentation "The lesser link of a ordering-pair.")
     (greater-link :initarg :greater-link
		   :initform nil
		   :reader lord=get-greater-link
		   :writer lord=set-greater-link!
		   :documentation "The greater link of a ordering-pair."))))

(defmethod print-object ((object lord+ordering-pair) stream)
  (format stream "(~A is smaller than ~A)"
	  (lord=get-lesser-link object)
	  (lord=get-greater-link object)))

(defun lord~create-ordering-pair (lesser-link greater-link)
  (make-instance 'lord+ordering-pair
		 :lesser-link lesser-link
		 :greater-link greater-link))

(eval-when (load compile eval)
  (defclass lord+trail (keim+object)
    ((start-literal :initarg :start-literal
		    :initform nil
		    :reader lord=get-start-literal
		    :writer lord=set-start-literal!
		    :documentation "The literal from whom thw trail starts.")
     (link-list :initarg :link-list
		:initform nil
		:reader lord=get-link-list
		:writer lord=set-link-list!
		:documentation "The current list of links to follow, this is in the rigth order, that means the first link is the link with start
literal")
     (end-literal :initarg :end-literal
		  :initform nil
		  :reader lord=get-end-literal
		  :writer lord=set-end-literal!
		  :documentation "The current end-literal of the trail, last link is always a link with the clause of end-literal."))))
  
(defmethod  print-object ((object lord+trail) stream)
  (format stream "(~A trailed with ~A)"
	  (lord=get-start-literal object)
	  (lord=get-end-literal object)))

(defun lord~create-trail (start-literal link-list end-literal)
  (make-instance 'lord+trail
		 :start-literal start-literal
		 :link-list link-list
		 :end-literal end-literal)) 

#| ----------------------------------------------------------- Auxiliaries --------------------------------------------------------- |#

(defun lord=create-initial-trail (start-literal)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A literal.")
	   (effect  "None.")
	   (value   "A trail with input-literal as start-literal and end-literal and nil in"
		    "used-link slot."))
  (lord~create-trail start-literal nil start-literal))

(defun lord=update-trail (trail new-link new-literal-list) 
  (declare (edited  "02-AUG-1996")
	   (authors Ameier)
	   (input   "A trail and a new links and a list of new literals."
		    "The link has end-literal of trail in its shore and the literals"
		    "are all literals reacheble in a trail (that means in the different"
		    "shore as end-literal) from end-literal and the new link.")
	   (effect  "None.")
	   (value   "A list of new trails:"
		    "For each literal of the input list a new trail is constructed with:"
		    "same start-literal as input-trail, the link-list from input trail is"
		    "updated by the new-link and the end-literal is the according literal." ))
  (let* ((start-literal (lord=get-start-literal trail))
	 (link-list (lord=get-link-list trail)))
    (mapcar #'(lambda (new-literal)
		(lord~create-trail start-literal (append link-list (list new-link)) new-literal))
	    new-literal-list)))

(defun lord=expandable-polylink-p (polylink)
  (declare (edited  "05-AUG-1996")
	   (authors Ameier)
	   (input   "A list of polylinks, a list of link-ordering pairs and two lists of links."
		    "The first list accord to the one side of literals of a splittet"
		    "clause, the other link-list acoords to the second side.")
	   (effect  "None.")
	   (value   "T if literals of the polylink are still ground or ground-instantiations"
		    "without skolems and (if only one shore is poly) the single part isn't trivial."
		    "Nil otherwise."))
  (and (lord=literals-ground-p polylink)
       (lord=polylink-not-trivial-p polylink)))

(defun lord=polylink-not-trivial-p (polylink)
  (declare (edited  "05-AUG-1996")
	   (authors Ameier)
	   (input   "A polylink.")
	   (effect  "None.")
	   (value   "T if the polylink is a double-polylink (both shores poly), or if"
		    "the clause at the single-shore is bigger than 1."))
  (if (plco~double-polylink-p polylink)
      t
    (let* ((neg-shore (ref~negative-shore polylink))
	   (pos-shore (ref~positive-shore polylink))
	   (single-shore (if (> (length neg-shore) 1)
			     pos-shore
			   neg-shore)))
      (if (= (length (cl~literals (lit~clause (first single-shore)))) 1)
	  nil
	t))))

(defun lord=literals-ground-p (polylink)
  (declare (edited  "05-AUG-1996")
	   (authors Ameier)
	   (input   "A polylink.")
	   (effect  "None.")
	   (value   "T if the literals at the link are ground or ground-instantiatable"
		    "without skolem-constants, or to say it other if all skolems in the literals"
		    "at the link are yet instantiated."))
  (let* ((literals (append (ref~positive-shore polylink)
			   (ref~negative-shore polylink)))
	 (first-lit (first literals))
	 (ground-lit (subst~apply (r2ntop~trans-clause-ground-subst (lit~clause first-lit)) first-lit))
	 (skolems (remove-duplicates r2ntop*skolem-constants)))
    (do* ((rest-skolems skolems (rest rest-skolems))
	  (flag  (null (data~substruct-positions (first rest-skolems) ground-lit))
		 (null (data~substruct-positions (first rest-skolems) ground-lit))))
	((or (null rest-skolems) (null flag)) flag))))



#| -------------------------------------------------------------- MAIN ------------------------------------------------------------ |#

(defun lord~check-unit-before-splitting! (unit node-to-split)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A decompose unit and a pdsnode (to be split).")
	   (effect  "There is computed wether there would be duplication of refutation graph parts"
		    "if the node would be splitted. If that's the case there are TND added"
		    "in the refutation graph and in the nd-proof to be split before"
		    "this node should be splittet. The order in which these nodes has to"
		    "become splitted is stored in the pre-selected-nodes slot of the"
		    "decompose unit.")
	   (value   "Undefined."))
  (omega~message "~%PREPARING DECOMPOSE UNIT FOR SPLITTING ...~%")
  
  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (ref-graph (r2ntop~decompose-unit-ref-graph unit))
	 (links (ref~links ref-graph))
	 (polylinks (remove-if-not #'plco~polylink-p links))
	 (usable-polylinks (remove-if-not #'lord=expandable-polylink-p polylinks))
	 (pairs-of-node-to-split (remove-if-not #'(lambda (pair)
						    (eq (extdelta~pdsnode pair) node-to-split))
						(extdelta~relation-pairs delta-relation)))
	 (clauses-to-split (remove-duplicates (mapcar #'extdelta~clause pairs-of-node-to-split)))
	 (list-of-literal-lists1 (split~get-list-of-literal-lists-with-position clauses-to-split pairs-of-node-to-split
										(pos~list-position '(1))))
	 (list-of-literal-lists2 (split~get-list-of-literal-lists-with-position clauses-to-split pairs-of-node-to-split
										(pos~list-position '(2))))
	 (polylinks-to-expand
	  (remove-duplicates
	   (apply 'append (mapcar #'(lambda (literal-list1 literal-list2)
				      (lord=get-polylinks-to-expand literal-list1 literal-list2 links usable-polylinks))
				  list-of-literal-lists1 list-of-literal-lists2))))
	 (ordered-polylinks (lord=order-polylinks-to-expand polylinks-to-expand)))
    (mapcar #'(lambda (polylink)
		(keim~put polylink 'greater-polylinks nil))
	    polylinks)
    (lord=expand-polylinks! ordered-polylinks unit node-to-split)))

(defun lord=get-polylinks-to-expand (literal-list1 literal-list2 links polylinks)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "Two lists of literals , a list of links and a list of polylinks." )
	   (effect  "None.")
	   (value   "A list of polylinks that would became doubled if the clause"
		    "of the literal-lists is splittet."))
  (let* ((initial-trails1 (mapcar #'lord=create-initial-trail literal-list1))
	 (initial-trails2 (mapcar #'lord=create-initial-trail literal-list2))
	 (all-trails-of-literals1 (apply 'append (mapcar #'(lambda (initial-trail)
							     (lord=get-all-trails initial-trail links))
							 initial-trails1)))
	 (all-trails-of-literals2 (apply 'append (mapcar #'(lambda (initial-trail)
							     (lord=get-all-trails initial-trail links))
							 initial-trails2))))
    (remove-if-not #'(lambda (polylink)
		       (lord=doubling-polylink-p polylink all-trails-of-literals1 all-trails-of-literals2))
		   polylinks)))

(defun lord=doubling-polylink-p (polylink trail-list1 trail-list2)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A polylink and two list of trails.")
	   (effect  "The polylink's plist is changed, the greater polylinks (see value) are appended"
		    "to the greater-polylinks slot.")
	   (value   "T if the polylink has to be splittet in case of splitting the clause"
		    "according to the trail-lists, nil otherwise."
		    "This is the case iff there exists two different literals in a polyshore of"
		    "polylink, so that for the first literal exists a trail from the first list"
		    "of trails and for the second literal exists a trail from the second list of"
		    "trails and these trails have no used link in common."
		    "If such trail pairs exists you can compute a list of greater polylinks, this"
		    "is a list of all polylinks who are in the links of the used-links of such a"
		    "trail-pair (see effect)."))
  (cond ((plco~double-polylink-p polylink)
	 (let* ((neg-shore (ref~negative-shore polylink))
		(pos-shore (ref~positive-shore polylink))
		(trails-ending-in-posshore-from1 (lord=get-trails-ending-in-one-of-literals trail-list1 pos-shore))
		(trails-ending-in-posshore-from2 (lord=get-trails-ending-in-one-of-literals trail-list2 pos-shore))
		(trails-ending-in-negshore-from1 (lord=get-trails-ending-in-one-of-literals trail-list1 neg-shore))
		(trails-ending-in-negshore-from2 (lord=get-trails-ending-in-one-of-literals trail-list2 neg-shore))
		(corresponding-trail-pairs-pos-shore
		 (lord=get-corresponding-trail-pairs pos-shore trails-ending-in-posshore-from1 trails-ending-in-posshore-from2))
		(corresponding-trail-pairs-neg-shore
		 (lord=get-corresponding-trail-pairs neg-shore trails-ending-in-negshore-from1 trails-ending-in-negshore-from2))
		(greater-polylinks (lord=get-polylinks-on-trails (append corresponding-trail-pairs-pos-shore
									 corresponding-trail-pairs-neg-shore))))
	   (if (or corresponding-trail-pairs-pos-shore corresponding-trail-pairs-neg-shore)
	       (progn
		 (keim~put polylink 'greater-polylinks (remove-duplicates (append (keim~get polylink 'greater-polylinks)
										  greater-polylinks)))
		 t)
	     nil)))
	(t
	 (let* ((neg-shore (ref~negative-shore polylink))
		(pos-shore (ref~positive-shore polylink))
		(polyshore (if (> (length neg-shore) 1)
			       neg-shore
			     pos-shore))
		(trails-ending-in-polyshore-from1 (lord=get-trails-ending-in-one-of-literals trail-list1 polyshore))
		(trails-ending-in-polyshore-from2 (lord=get-trails-ending-in-one-of-literals trail-list2 polyshore))
		(corresponding-trail-pairs
		 (lord=get-corresponding-trail-pairs polyshore trails-ending-in-polyshore-from1 trails-ending-in-polyshore-from2))
		(greater-polylinks (lord=get-polylinks-on-trails corresponding-trail-pairs)))
	   (if corresponding-trail-pairs
	       (progn
		 (keim~put polylink 'greater-polylinks (remove-duplicates (append (keim~get polylink 'greater-polylinks)
										  greater-polylinks)))
		 t)
	     nil)))))

(defun lord=get-polylinks-on-trails (trail-pair-list)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A list of trial-pairs.")
	   (effect  "None.")
	   (value   "A list of all polylinks, standing in a trail's link-list."))
  (let* ((links (remove-duplicates (apply 'append (mapcar #'(lambda (trail-pair)
							      (append (lord=get-link-list (first trail-pair))
								      (lord=get-link-list (second trail-pair))))
							  trail-pair-list)))))
    (remove-if-not #'plco~polylink-p links)))
				       
(defun lord=get-corresponding-trail-pairs (polyshore trails-ending-in-polyshore-from1 trails-ending-in-polyshore-from2)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A list of literals representing a polyshore of a polylink, two lists of trails ending"
		    "in literals of this polyshore.")
	   (effect  "None.")
	   (value   "A list of trail-pairs (one from first trail-list ,other from second),with different end-literals"
		    "and no used-link in common."))
  (apply 'append
	 (mapcar #'(lambda (literal)
		     (let* ((trails-ending-with-this-literal-from1 (remove-if-not #'(lambda (trail)
										      (eq literal (lord=get-end-literal trail)))
										  trails-ending-in-polyshore-from1))
			    (trails-ending-in-other-literals-from2 (remove-if #'(lambda (trail)
										  (eq literal (lord=get-end-literal trail)))
									      trails-ending-in-polyshore-from2)))
		       (apply 'append
			      (mapcar #'(lambda (trail-ending-with-this-literal-from1)
					  (let* ((trails-ending-in-other-literals-from2-and-no-used-link-in-common
						  (remove-if #'(lambda (trail-from2)
								 (intersection
								  (lord=get-link-list trail-from2)
								  (lord=get-link-list trail-ending-with-this-literal-from1)))
							     trails-ending-in-other-literals-from2)))
					    (mapcar #'(lambda (trail-from2)
							(list trail-ending-with-this-literal-from1 trail-from2))
						    trails-ending-in-other-literals-from2-and-no-used-link-in-common)))
				      trails-ending-with-this-literal-from1))))
		 polyshore)))
			        
(defun lord=get-trails-ending-in-one-of-literals (list-of-trails literal-list)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A list of trais and a list of literals.")
	   (effect  "None.")
	   (value   "A list of trails, ending in one of the literals of literal-list."))
  (remove-if-not #'(lambda (trail)
		     (member (lord=get-end-literal trail) literal-list))
		 list-of-trails))
		    
(defun lord=get-all-trails (trail links)
  (declare (edited  "07-AUG-1996")
	   (authors Ameier)
	   (input   "A Trail and a set of links.")
	   (effect  "None.")
	   (value   "A list of all trails beginning with this trail and goimg further"
		    "with the end-literal of the trail."))
  (let* ((end-literal (lord=get-end-literal trail))
	 (usable-links (r2ntop~remove-list (lord=get-link-list trail) links)))
    (multiple-value-bind
	(new-link contra-literals)
	(r2ntop~get-link-and-contra-literals end-literal usable-links)
      (if new-link
	  (let* ((all-new-reachable-literals (remove-duplicates (apply 'append (mapcar #'cl~literals
										       (mapcar #'lit~clause contra-literals))))))
	    (cons trail (apply 'append (mapcar #'(lambda (new-trail)
						   (lord=get-all-trails new-trail links))
					       (lord=update-trail trail new-link all-new-reachable-literals)))))
	(list trail)))))


(defun lord=expand-polylinks! (list-of-ordered-polylinks unit node-to-split)
  (declare (edited  "05-AUG-1996")
	   (authors Ameier)
	   (input   "A list of polylinks to expand and to split, the current decompose unit and"
		    "the current-node-to-split.")
	   (effect  "The polylinks are expanded in the refutation-graph of unit and the"
		    "so generated nodes are set to the selected-nodes slot of unit.")
	   (value   "Undefined."))
  (let* ((ref-graph (r2ntop~decompose-unit-ref-graph unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (trivial-lemma-nodes (mapcar #'(lambda (polylink)
					  (r2ntop~insert-lemma-in-proof! polylink ref-graph delta-relation))
				      list-of-ordered-polylinks)))
    ;; is das ok ???
    (setf (r2ntop~decompose-unit-nodes-to-decompose unit)
	  (append trivial-lemma-nodes (list node-to-split)))
    ;; braucht man das ???
    
    (setf (r2ntop~decompose-unit-pre-selected-nodes unit)
	  (append trivial-lemma-nodes (list node-to-split)))))

(defun lord=order-polylinks-to-expand (polylinks)
  (declare (edited  "05-AUG-1996")
	   (authors Ameier)
	   (input   "A list of polylinks.")
	   (effect  "None.")
	   (value   "The list of polylinks ordered after the contain of the plist slot"
		    "greater-polylinks, that means:"
		    "If polylink p1 is smaller as polylink p2 it will stand in the"
		    "return list before p2. So you obtain a sequance to (expand and) split"
		    "the polylinks."))
  (if polylinks
      (let* ((smallest-polylinks (lord=get-smallest-polylinks polylinks)))
	(append smallest-polylinks (lord=order-polylinks-to-expand (r2ntop~remove-list smallest-polylinks polylinks))))
    nil))

(defun lord=get-smallest-polylinks (polylinks)
  (declare (edited  "05-AUG-1996")
	   (authors Ameier)
	   (input   "A list of polylinks.")
	   (effect  "Nil.")
	   (value   "A list of all polylinks from the input list, who doesn't stand"
		    "in an other polylinks  greater-polylinks plist slot, as greater"
		    "That means polylinks who have according to the order with this"
		    "greater-polylinks no smaller ones."))
  (do* ((rest-polylinks polylinks (rest rest-polylinks))
	(smallest-polylinks polylinks))
      ((null rest-polylinks) smallest-polylinks)
    (let* ((greater-polylinks (keim~get (first rest-polylinks) 'greater-polylinks)))
      (if (member (first rest-polylinks) smallest-polylinks)
	  (setq smallest-polylinks (r2ntop~remove-list greater-polylinks smallest-polylinks))))))







