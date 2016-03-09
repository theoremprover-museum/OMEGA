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




(mod~defmod PLCO 
            :uses (keim lit node omega r2ntop ref)
            :documentation "Computing contradictional polylinks."
            :exports (
                      plco+compare-pair
                      plco+d2s-link
                      plco+link-parts
                      
                      plco~attribut
                      plco~compute-compare-pair-list
                      plco~create-compare-pair
                      plco~create-d2s-link
                      plco~create-d2s-links
                      plco~create-link-parts
                      plco~double-polylink-p
                      plco~first-polylink
                      plco~g-part-polylinks
                      plco~get-minimal-polylinks
                      plco~k-part-polylinks
                      plco~poly-shore
                      plco~polylink
                      plco~polylink-p
                      plco~second-polylink
                      plco~source-link
                      plco~sspu-refutable-p
                      plco~update-plcos
                      
                      plco*d2s-links))








(defvar plco*d2s-links nil)

#| ---------------------------------------------------- DATASTRUCTS ---------------------------------------------------- |#

;; plco+compare-pair
;; Attention: The shore-slots in plco+compare-pair and plco+link-parts are always the shores of the
;;            according g-part, for a double link you receive two links-parts


(eval-when (load compile eval)
  (defclass plco+compare-pair (keim+object)
    ((attribut :initarg :attribut
	       :initform nil
	       :accessor plco~attribut
	       :documentation "The attribut between the two links.")
     (first-polylink :initarg :first-polylink
		     :initform nil
		     :accessor plco~first-polylink
		     :documentation "The first polylink of a compare pair")
     (second-polylink :initarg :second-polylink
		      :initform nil
		      :accessor plco~second-polylink
		      :documentation "The second polylink of a compare pair"))))

(defmethod print-object ((object plco+compare-pair) stream)
  (format stream "(~A is ~A to ~A)"
	  (plco~first-polylink object)
	  (plco~attribut object)
	  (plco~second-polylink object)))

(defgeneric plco~create-compare-pair (first-link second-link attribut)
  (declare (edited  "02-MAY-1996")
	   (authors Ameier)
	   (input   "Two links.")
	   (effect  "None.")
	   (value   "A new plco+compare-pair is created, with first input link as"
		    "first polylink and second input link as second."))
  (:method ((first-link ref+link) (second-link ref+link) attribut)
	   (if (or (equal attribut '<) (equal attribut '><) (equal attribut '=))
	       (make-instance 'plco+compare-pair
			      :attribut attribut
			      :first-polylink first-link
			      :second-polylink second-link)
	     (omega~error "~A isn't a aollowed attribut between two poly-links." attribut))))



;; plco+link-parts

(eval-when (load compile eval)
  (defclass plco+link-parts (keim+object)
    ((polylink :initarg :polylink
	       :initform nil
	       :accessor plco~polylink
	       :documentation "The polylink um dens hier geht.")
     (k-part-polylinks :initarg :k-part-polylinks
		       :initform nil
		       :accessor plco~k-part-polylinks
		       :documentation "The list of all polylinks in the k-part of the polylink.")
     (g-part-polylinks :initarg :g-part-polylinks
		       :initform nil
		       :accessor plco~g-part-polylinks
		       :documentation "The list of all polylinks in the g-part of the polylink."))))

(defmethod print-object ((object plco+link-parts) stream)
  (format stream "(Polylink: ~A , K-part: ~A , G-part : ~A)"
	  (plco~polylink object)
	  (plco~k-part-polylinks object)
	  (plco~g-part-polylinks object)))

(defun plco~create-link-parts (link k-part-list g-part-list)
  (make-instance 'plco+link-parts
		 :polylink link
		 :k-part-polylinks k-part-list
		 :g-part-polylinks g-part-list))

;;plco+d2s--link

(eval-when (load compile eval)
  (defclass plco+d2s-link (ref+link)
    ((source-link :initarg :source-link
		  :initform nil
		  :accessor plco~source-link
		  :documentation "In this slot stand the source double poly-link from which this plco+d2s-link is created.")
     (poly-shore :initarg :poly-shore
		 :initform nil
		 :accessor plco~poly-shore
		 :documentation "This slot says which shore of the d2s-link is in this instance the double. t for pos-shore nil-for-neg-shore."))))

(defun plco~create-d2s-link (link shore)
  (make-instance 'plco+d2s-link
		 :name (if shore
			   (format nil "+~A+" (keim~name link))
			 (format nil "+~A+" (keim~name link)))
		 :positive-shore (ref~positive-shore link)
		 :negative-shore (ref~negative-shore link)
		 :substitution (ref~substitution link)
		 :source-link link
		 :poly-shore shore))

(defun plco~create-d2s-links (link)
  (list (plco~create-d2s-link link 't) (plco~create-d2s-link link nil)))

  
(defmethod print-object ((object plco+d2s-link) stream)
  (format stream "[D2S-LINK: FROM ~A ,poly-shore ~A]"
	  (plco~source-link object)
	  (if (plco~poly-shore object)
	      '+
	    '-)))

#| ---------------------------------------------------- AUXILIARIES ---------------------------------------------------- |#

(defgeneric plco~polylink-p (link)
  (declare (edited  "02-MAY-1996")
	   (authors Ameier)
	   (input   "A link.")
	   (effect  "None.")
	   (value   "t if atleast one shore is bigger than 1 element."))
  (:method ((link plco+d2s-link))
	   (if (plco~poly-shore link)
	       ;; + shore should be poly poly
	       (if (> (length (ref~positive-shore link)) 1)
		   't
		 nil)
	     ;; - shore should be poly
	     (if (> (length (ref~negative-shore link)) 1)
		 't
	       nil)))
  (:method ((link ref+link))
	   (or (> (length (ref~positive-shore link)) 1)
	       (> (length (ref~negative-shore link)) 1))))

(defun plco~double-polylink-p (link)
  (declare (edited  "07-MAY-1996")
	   (authors Ameier)
	   (input   "A link.")
	   (effect  "None.")
	   (value   "t if both shores of the link are bigger than 1 element."))
  (and (> (length (ref~positive-shore link)) 1)
       (> (length (ref~negative-shore link)) 1)))

#| --------------------------------------------- Compute polylink compare-pairs ---------------------------------------- |#

(defun plco~compute-compare-pair-list (ref-graph)
  (declare (edited  "02-MAY-1996")
	   (authors Ameier)
	   (input   "A refutation-graph.")
	   (effect  "None." )
	   (value   "Computes the list of all compare-pairs between the"
		    "polylinks of the ref-graph."))
  (let* ((links (ref~links ref-graph))
	 (polylinks (remove-if-not #'plco~polylink-p links))
	 (double-polylinks (remove-if-not #'plco~double-polylink-p polylinks))
	 (single-polylinks (r2ntop~remove-list double-polylinks polylinks))
	 (d2s-links (apply 'append (mapcar #'plco~create-d2s-links double-polylinks)))
	 (all-polylinks (append single-polylinks d2s-links)))
    (setq plco*d2s-links d2s-links)
    (let ((list-of-link-parts (mapcar #'(lambda (polylink)
					  (plco=compute-parts-of-polylink polylink polylinks ref-graph))
				      all-polylinks)))
      (append
       (apply 'append
	      (mapcar #'(lambda (link-parts)
			  (plco=link-parts-2-compare-pairs link-parts list-of-link-parts))
		      list-of-link-parts))
       (do* ((rest-d2s-links d2s-links (rest (rest rest-d2s-links)))
	     (compare-pairs nil))
	   ((null rest-d2s-links) compare-pairs)
	 (setq compare-pairs (cons (plco~create-compare-pair (first rest-d2s-links)
							     (second rest-d2s-links) '><) compare-pairs)))))))
    

(defun plco=link-parts-2-compare-pairs (link-parts list-of-link-parts)
  (declare (edited  "07-MAY-1996")
	   (authors Ameier)
	   (input   "A link-parts instance, and a list of link-parts.")
	   (effect  "None.")
	   (value   "A list of compare-pairs."))
  (let ((link (plco~polylink link-parts))
	(k-part (plco~k-part-polylinks link-parts))
	(g-part (plco~g-part-polylinks link-parts)))
    (append 
     (mapcar #'(lambda (k-part-link)
		 (let* ((k-link-parts (first (member k-part-link list-of-link-parts
						     :test #'(lambda (item1 item2)
							       (eq item1 (plco~polylink item2))))))
			(k-link (plco~polylink k-link-parts))
			(k-link-k-part (plco~k-part-polylinks k-link-parts))
			(k-link-g-part (plco~g-part-polylinks k-link-parts)))
		   (cond ((member link k-link-k-part)
			  (setf (plco~k-part-polylinks k-link-parts) (remove link k-link-k-part)) 
			  (plco~create-compare-pair link k-link '><))
			 ((member link k-link-g-part)
			  (setf (plco~g-part-polylinks k-link-parts) (remove link k-link-g-part))
			  (plco~create-compare-pair k-link link '<)))))
	     k-part)
     (mapcar #'(lambda (g-part-link)
		 (let* ((g-link-parts (first (member g-part-link list-of-link-parts
						     :test #'(lambda (item1 item2)
							       (eq item1 (plco~polylink item2))))))
			(g-link (plco~polylink g-link-parts))
			(g-link-k-part (plco~k-part-polylinks g-link-parts))
			(g-link-g-part (plco~g-part-polylinks g-link-parts)))
		   (cond ((member link g-link-k-part)
			  (setf (plco~k-part-polylinks g-link-parts) (remove link g-link-k-part))
			  (plco~create-compare-pair link g-link '<))
			 ((member link g-link-g-part)
			  (setf (plco~g-part-polylinks g-link-parts) (remove link g-link-g-part))
			  (plco~create-compare-pair g-link link '=)))))
	     g-part))))

(defgeneric plco=compute-parts-of-polylink (polylink polylinks ref-graph)
  (declare (edited  "02-MAY-1996")
	   (authors Ameier)
	   (input   "A polylink ,the list of all polylinks and the ref-graph.")
	   (effect  "None.")
	   (value   "Computes the parts of the polylink and creates new"
		    "plco+link-parts. A list of these is returned."))
  (:method ((polylink plco+d2s-link) polylinks ref-graph)
	   (declare (ignore polylinks))
	   (let* ((poly-shore (plco~poly-shore polylink))
		  (k-part-clauses (if poly-shore
				      (remove-duplicates (mapcar #'lit~clause (ref~negative-shore polylink)))
				    (remove-duplicates (mapcar #'lit~clause (ref~positive-shore polylink))))))
	     (multiple-value-bind
		 (k-part-polylinks g-part-polylinks)
		 (plco=get-parts-from-clauses (plco~source-link polylink)
					      k-part-clauses nil (list (plco~source-link polylink))
					      nil ref-graph)
	       (plco~create-link-parts polylink
				       (plco=del-double-polylinks k-part-polylinks)
				       (plco=del-double-polylinks g-part-polylinks)))))
  (:method ((polylink ref+link) polylinks ref-graph)
	   (declare (ignore polylinks))
	   (let* ((poly-shore (if (> (length (ref~positive-shore polylink)) 1)
				  't
				nil))
		  (k-part-clauses (if poly-shore
				      (remove-duplicates (mapcar #'lit~clause (ref~negative-shore polylink)))
				    (remove-duplicates (mapcar #'lit~clause (ref~positive-shore polylink))))))
	     (multiple-value-bind
		 (k-part-polylinks g-part-polylinks)
		 (plco=get-parts-from-clauses polylink k-part-clauses nil (list polylink) nil ref-graph)
	       (plco~create-link-parts polylink
				       (plco=del-double-polylinks k-part-polylinks)
				       (plco=del-double-polylinks g-part-polylinks))))))

(defun plco=del-double-polylinks (list-of-links)
  (declare (edited  "09-MAY-1996")
	   (authors Ameier)
	   (input   "A list of links.")
	   (effect  "None.")
	   (value   "If a link is a double-polylink L, it is replaced by"
		    "the two according single-polylinks L+ and L-, standing"
		    "in the global-variable plco*d2s-links."))
  (do* ((rest-list-of-links list-of-links (rest rest-list-of-links))
	(return-list-of-links nil))
      ((null rest-list-of-links) return-list-of-links)
    (let ((head-link (first rest-list-of-links)))
      (if (plco~double-polylink-p head-link)
	  (setq return-list-of-links (append (remove-if-not #'(lambda (d2s-link)
								(eq (plco~source-link d2s-link) head-link))
							    plco*d2s-links)
					     return-list-of-links))
	(setq return-list-of-links (cons head-link return-list-of-links))))))

(defun plco=get-parts-from-clauses (polylink current-k-part-clauses checked-k-part-clauses
					     checked-links k-part-links ref-graph)
  (if current-k-part-clauses
      (multiple-value-bind
	  (new-clauses new-links)
	  (plco=get-new-clauses-and-new-links current-k-part-clauses checked-k-part-clauses checked-links ref-graph)
	(plco=get-parts-from-clauses polylink
				     new-clauses
				     (append current-k-part-clauses checked-k-part-clauses)
				     (append new-links checked-links)
				     (append (remove-if-not #'plco~polylink-p new-links) k-part-links)
				     ref-graph))
    (let ((polylinks (remove-if-not #'plco~polylink-p (ref~links ref-graph))))
      (values k-part-links
	      (r2ntop~remove-list (cons polylink k-part-links) polylinks))))) 

(defun plco=get-new-clauses-and-new-links (current-k-part-clauses checked-clauses checked-links ref-graph)
  (let* ((links (ref~links ref-graph))
	 (list-of-new-clauses-and-new-links 
	  (mapcar #'(lambda (clause)
		      ;; compute the new links of this clause
		      (let ((new-links-with-this-clause
			     (r2ntop~remove-list checked-links
						 (remove-if-not #'(lambda (link)
								    (let ((literals (append (ref~positive-shore link)
											    (ref~negative-shore link))))
								      (member clause (mapcar #'lit~clause literals))))
								links))))
			(list
			 ;; compute all new clauses in opposite shores of clause
			 (r2ntop~remove-list
			  (append current-k-part-clauses checked-clauses)
			  (remove-duplicates
			   (apply 'append
				  (mapcar #'(lambda (new-link)
					      (let ((pos-shore-clauses (mapcar #'lit~clause (ref~positive-shore new-link)))
						    (neg-shore-clauses (mapcar #'lit~clause (ref~negative-shore new-link))))
						(if (member clause pos-shore-clauses)
						    neg-shore-clauses
						  pos-shore-clauses)))
					  new-links-with-this-clause))))
			 new-links-with-this-clause)))
		  current-k-part-clauses)))
    (values
     ;; the new clauses
     (remove-duplicates (apply 'append (mapcar #'first list-of-new-clauses-and-new-links)))
     ;; the new links
     (remove-duplicates (apply 'append (mapcar #'second list-of-new-clauses-and-new-links))))))


#| ------------------------------------------------ Get minimal Polylinks --------------------------------------------- |#

(defun plco~get-minimal-polylinks (list-of-polylink-compare-pairs)
  (declare (edited  "02-MAY-1996")
	   (authors Ameier)
	   (input   "A list of polylink-compare-pairs.")
	   (effect  "None.")
	   (value   "A list of all minimal polylinks."))
  (do* ((rest-compare-pairs list-of-polylink-compare-pairs (rest rest-compare-pairs))
	(minimal-links nil)
	(non-minimal-links nil))
      ((null rest-compare-pairs) minimal-links)
    (let* ((head (first rest-compare-pairs))
	   (first-polylink (plco~first-polylink head))
	   (second-polylink (plco~second-polylink head))
	   (attribut (plco~attribut head)))
      (cond ((equal attribut '><)
	     (setq non-minimal-links (remove-duplicates (append (list first-polylink second-polylink) non-minimal-links)))
	     (setq minimal-links (r2ntop~remove-list (list first-polylink second-polylink) minimal-links)))
	    ((equal attribut '<)
	     (if (not (member first-polylink non-minimal-links))
		 (if (not (member first-polylink minimal-links))
		     (setq minimal-links (cons first-polylink minimal-links))))
	     (if (not (member second-polylink non-minimal-links))
		 (setq non-minimal-links (cons second-polylink non-minimal-links)))
	     (if (member second-polylink minimal-links)
		 (setq minimal-links (remove second-polylink minimal-links))))
	    (t ;; '=
	     (mapcar #'(lambda (link)
			 (if (not (member link non-minimal-links))
			     (if (not (member link minimal-links))
				 (setq minimal-links (cons link minimal-links)))))
		     (list first-polylink second-polylink)))))))

#| ------------------------------------------------- sspu-refutable-p -------------------------------------------------- |#

(defun plco~sspu-refutable-p (plco-list)
  (declare (edited  "09-MAY-1996")
	   (authors Ameier)
	   (input   "A polylink-comparison-list.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: t if now plco has attribut ><,otherwise nil."
		    "Second: if first is nil, a list of all plco's who have"
		    "        >< as attribut.")) 
  (do* ((rest-plco-list plco-list (rest rest-plco-list))
	(flag 't)
	(contradiction-list nil))
      ((null rest-plco-list) (values flag contradiction-list))
    (let ((head-plco (first rest-plco-list)))
      (if (equal (plco~attribut head-plco) '><)
	  (progn
	    (setq flag nil)
	    (setq contradiction-list (cons head-plco contradiction-list)))))))

#| ----------------------------------------------- update plco-pairs --------------------------------------------------- |#

#|

Momentan nicht benuetzt,...
sollte man aber vielleicht !
DAzu noch anpassen: unit statt triple usw.
|#

#|
(defun plco~update-plcos (new-node old-triple)
  (declare (edited  "08-MAY-1996")
	   (authors Ameier)
	   (input   "The new-node (in its justification the new ref-graph) and the old"
		    "decompose-triple ,which contain the old-plco-list.")
	   (effect  "None.")
	   (value   "The plco-list of the first ref-graph is computed."))
  (let* ((new-ref-graph (r2ntop~refutation-graph (node~justification new-node)))
	 (link-pair-list (keim~get new-ref-graph 'link-pair-list))
	 (old-plco-list (r2ntop~decompose-unit-plco-pairs old-triple)))
    (keim~put new-ref-graph 'link-pair-list nil)
    (apply `append
	   (mapcar #'(lambda (old-plco)
		       (let* ((first-plink (plco~first-polylink old-plco))
			      (second-plink (plco~second-polylink old-plco))
			      (attribut (plco~attribut old-plco))
			      (acc-new-first-plink (plco=get-according-new-link first-plink link-pair-list))
			      (acc-new-second-plink (plco=get-according-new-link second-plink link-pair-list)))
			 (if (and
			      acc-new-first-plink acc-new-second-plink
			      (plco~polylink-p acc-new-first-plink) (plco~polylink-p acc-new-second-plink))
			     (list (plco~create-compare-pair (plco=check-link acc-new-first-plink)
							     (plco=check-link acc-new-second-plink) attribut))
			   nil)))
		   old-plco-list)))) 

(defgeneric plco=check-link (link)
  (declare (edited  "09-MAY-1996")
	   (authors Ameier)
	   (input   "A link.")
	   (effect  "None.")
	   (value   "If link is ref+link the link himself."
		    "If link is plco+d2s-link, it is checked wether the link"
		    "is already a double-polylink, if so the link himself is"
		    "returned, otherwise the source-link of plco+d2s-link is"
		    "returned,cause you doesn't need a plco+d2s-link."))
  (:method ((link ref+link))
	   link)
  (:method ((link plco+d2s-link))
	   (let ((source-link (plco~source-link link)))
	     (if (plco~double-polylink-p source-link)
		 link
	       source-link)))) 
  
(defgeneric plco=get-according-new-link (old-link link-pair-list)
  (declare (edited  "09-MAY-1996")
	   (authors Ameier)
	   (input   "A (old) link and a list of pairs (old-link/new-link).")
	   (effect  "None.")
	   (value   "Seeks the according link from the list of links-pairs"
		    "according to old-link."))
  (:method ((old-link plco+d2s-link) link-pair-list)
	   (let* ((source-old-link (plco~source-link old-link))
		  (acc-new-link (second (first (member source-old-link link-pair-list
						       :test #'(lambda (link link-pair)
								 (eq link (first link-pair))))))))
	     (plco~create-d2s-link acc-new-link (plco~poly-shore old-link))))
  (:method ((old-link ref+link) link-pair-list)
	   (second (first (member old-link link-pair-list
				  :test #'(lambda (link link-pair)
					    (eq link (first link-pair))))))))


|#


