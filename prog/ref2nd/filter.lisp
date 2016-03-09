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

#| Dieses File entaelt Filter, die auf einen Resolutionsbeweis angewandt werden koennen, um diesen moeglichst zu vereinfachen.
   Bisher vorhanden: Filter Flip Schritte, um Flip Schritte zu eliminieren !|#

(in-package :omega)




(mod~defmod FILTER 
            :uses (cl data keim lit node pos res)
            :documentation "Filter to simplify resolution proofs."
            :exports (
                      
                      filter~remove-flip-steps!
                      filter~remove-flip-steps-from-resolution-proof!
                      filter~res-proof-contains-flip-steps-p
                      ))






(defun filter~res-proof-contains-flip-steps-p (res-proof)
  (declare (edited  "28-OCT-1997")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "T if the resolution proof contains flip steps."))
  (remove-if-not #'(lambda (clause)
		     (res~flip-p clause))
		 (res~proof-clauses res-proof)))

  
(defun filter~remove-flip-steps-from-resolution-proof! (res-proof)
  (declare (edited  "13-OCT-1997")
	   (authors Ameier)
	   (input   "A resolution-proof.")
	   (effect  "The proof is changed, while the algorthm tries to eliminate some"
		    "unecessary flip-steps. Espessially the clauses slot of the resolution"
		    "proof is changed and also some information (just,literals) in the"
		    "clauses themself.")
	   (value   "The changed resolution proof.")) 
  (let* ((empty-clause (res~proof-empty-clause res-proof)))

    (filter~remove-flip-steps! empty-clause)

    (setf (res~proof-clauses res-proof) (filter=get-ancestors empty-clause (node~justification empty-clause) 't))

    res-proof))
  
(defun filter~remove-flip-steps! (clause)
  (declare (edited  "10-OCT-1997")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "The plist entries unflip-clause and unflip-positions are changed."
		    "The hole proof tree above the clause is changed, while trying to"
		    "eliminate as many flip-step as possible. In the plist of the clauses"
		    "possibly the following entries are changed: unflip-clause, unflip-positions,"
		    "already-flipped-clauses, already-flipped-positions.")
	   (value   "Multiple value:"
		    "First: a clause in that all literals are unflipped if possible."
		    "Second: a list of positions of unflipped literals."
		    "Thereby the positions sign that literals, that are flipped in the returned"
		    "clause in comparison to the input-clause."))
  (if (keim~get clause 'unflip-clause)
      (values (keim~get clause 'unflip-clause)
	      (keim~get clause 'unflip-positions))
    (multiple-value-bind
	(unflip-clause unflip-positions)
	(filter=remove-flip-steps clause (node~justification clause))
      (keim~put clause 'unflip-clause unflip-clause)
      (keim~put clause 'unflip-positions unflip-positions)
      (values unflip-clause unflip-positions))))

(defgeneric filter=get-ancestors (clause just initials)
  (declare (edited  "28-OCT-1997")
	   (authors Ameier)
	   (input   "A clause and its justification a flag, to deceide, whether"
		    "the initial ancestors should be returned to or not (default is nil).")
	   (effect  "None.")
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
	   (let* ((parents (res~justification-parents just)))
	     (remove-duplicates
	      (append (apply 'append (mapcar #'(lambda (parent)
						 (filter=get-ancestors parent (node~justification parent) initials))
					     parents))
		      (list clause))
	      :from-end 't))))


(defgeneric filter=remove-flip-steps (clause just)
  (declare (edited  "10-OCT-1997")
	   (authors Ameier)
	   (input   "A clause and its justification.")
	   (effect  "None.")
	   (value   "Multiple value:"
		    "First: a clause in that all literals are unflipped if possible."
		    "Second: a list of positions of unflipped literals."
		    "Thereby the positions sign that literals, that are flipped in the returned"
		    "clause in comparison to the input-clause."))

  (:method (clause (just res+initial))
	   (values clause nil))

  (:method (clause (just res+reflex))
	   (values clause nil))

  (:method (clause (just res+flip))
	   (declare (ignore clause))
	   
	   (let* ((parent-clause (first (res~justification-parents just)))
		  (pos (first (res~justification-positions just))))
	     (multiple-value-bind
		 (back-clause back-pos-list)
		 (filter~remove-flip-steps! parent-clause)
	       
	       ;; ersetzten der parent-clause durch back-clause
	       (setf (res~justification-parents just) (list back-clause))
	       
	       (if (find pos back-pos-list :test 'keim~equal)
		   (values back-clause (remove pos back-pos-list :test 'keim~equal))
		 (values back-clause (cons pos back-pos-list))))))

  (:method (clause (just res+factoring))
	   (let* ((parent-clause (res~factoring-clause just))
		  (positions (res~factoring-positions just))
		  (pos1 (first positions))
		  (pos2 (second positions)))
	     (multiple-value-bind
		 (back-clause back-pos-list)
		 (filter~remove-flip-steps! parent-clause)

	       ;; ersetzten der parent-clause durch back-clause
	       (setf (res~justification-parents just) (list back-clause))
	       
	       (cond ((and (not (find pos1 back-pos-list :test 'keim~equal))
			   (not (find pos2 back-pos-list :test 'keim~equal)))
		      (let* ((flip-pos-list (filter=update-pos-list-factoring back-pos-list pos1 pos2)))

			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(values clause flip-pos-list))) 
		     ((and (find pos1 back-pos-list :test 'keim~equal)
			   (find pos2 back-pos-list :test 'keim~equal))
		      (let* ((flip-pos-list (filter=update-pos-list-factoring back-pos-list pos1 pos2 :add-fac-pos 't)))

			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(values clause flip-pos-list)))
		     ((and (find pos1 back-pos-list :test 'keim~equal)
			   (not (find pos2 back-pos-list :test 'keim~equal)))
		      (let* ((flip-pos-list (filter=update-pos-list-factoring back-pos-list pos1 pos2)))
			
			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(filter=insert-flip-step-between-clauses! just back-clause pos1) 
			(values clause flip-pos-list)))
		     (t
		      (let* ((flip-pos-list (filter=update-pos-list-factoring back-pos-list pos1 pos2)))
			
			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(filter=insert-flip-step-between-clauses! just back-clause pos2) 
			(values clause flip-pos-list)))))))

  (:method (clause (just res+resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (positions (res~resolution-positions just))
		  (parent1 (first parents))
		  (parent2 (second parents))
		  (pos1 (first positions))
		  (pos2 (second positions)))
	     (multiple-value-bind
		 (back-clause1 back-pos-list1)
		 (filter~remove-flip-steps! parent1)
	       (multiple-value-bind
		   (back-clause2 back-pos-list2)
		   (filter~remove-flip-steps! parent2)

		 ;; ersetzten der parent-clauses durch back-clauses
		 (setf (res~justification-parents just) (list back-clause1 back-clause2))
		 
		 (cond ((and (not (find pos1 back-pos-list1 :test 'keim~equal))
			     (not (find pos2 back-pos-list2 :test 'keim~equal)))

			(let* ((flip-pos-list (filter=update-pos-list-resolution parent1 back-pos-list1 back-pos-list2 pos1 pos2)))
			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)
			  
			  (values clause flip-pos-list))) 
		       ((and (find pos1 back-pos-list1 :test 'keim~equal)
			     (find pos2 back-pos-list2 :test 'keim~equal))
			
			(let* ((flip-pos-list (filter=update-pos-list-resolution parent1 back-pos-list1 back-pos-list2 pos1 pos2)))
			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)
			  
			  (values clause flip-pos-list)))
		       ((and (find pos1 back-pos-list1 :test 'keim~equal)
			     (not (find pos2 back-pos-list2 :test 'keim~equal)))

			(let* ((flip-pos-list (filter=update-pos-list-resolution parent1 back-pos-list1 back-pos-list2 pos1 pos2)))
			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)
			  
			  (filter=insert-flip-step-between-clauses! just back-clause1 pos1) 
			  (values clause flip-pos-list)))
		       (t

			(let* ((flip-pos-list (filter=update-pos-list-resolution parent1 back-pos-list1 back-pos-list2 pos1 pos2)))
			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)
			  
			  (filter=insert-flip-step-between-clauses! just back-clause2 pos2) 
			  (values clause flip-pos-list))))))))
  (:method (clause (just res+equality-factoring))
	   (let* ((parents (res~justification-parents just))
		  (positions (res~justification-positions just))
		  (parent (first parents))
		  (pos1 (first positions))
		  (pos2 (second positions))
		  (posonlyfirst1 (pos~list-position (list (pos~first pos1))))
		  (posonlyfirst2 (pos~list-position (list (pos~first pos2))))		
		  (remaining-position (res~equality-factoring-just-remaining-literal-position just)))
	     
	     (multiple-value-bind
		 (back-clause back-pos-list)
		 (filter~remove-flip-steps! parent)
	       
	       ;; ersetzten der parent-clauses durch back-clauses
	       (setf (res~justification-parents just) (list back-clause))
	       
	       (cond ((and (not (find posonlyfirst1 back-pos-list :test 'keim~equal))
			   (not (find posonlyfirst2 back-pos-list :test 'keim~equal)))
		      
		      ;; keines der beiden Literale wurde geflippt!
		      ;; Die Justification muss nicht weiter angepasst werden, aber die geflippten Positions muessen durchgegeben
		      ;; werden!
		      
		      (let* ((flip-pos-list (filter=update-pos-list-equality-factoring parent back-pos-list
										       posonlyfirst1 posonlyfirst2
										       remaining-position)))
			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(values clause flip-pos-list))) 
		     
		     ((and (find posonlyfirst1 back-pos-list :test 'keim~equal)
			   (find posonlyfirst2 back-pos-list :test 'keim~equal))
		      
		      ;; beide Literale wurden geflippt!
		      ;; Justification muss angepasst werden -> andere Seiten uniziert in positions
		      
		      (let* ((new-positions (list (pos~list-position (append (butlast (pos~number-list pos1))
									     (if (equal (first (last (pos~number-list pos1))) 1)
										 (list 2)
									       (list 1))))
						  (pos~list-position (append (butlast (pos~number-list pos2))
									     (if (equal (first (last (pos~number-list pos2))) 1)
										 (list 2)
									       (list 1))))))
			     (flip-pos-list (filter=update-pos-list-equality-factoring parent back-pos-list
										       posonlyfirst1 posonlyfirst2
										       remaining-position)))
			
			(setf (res~justification-positions just) new-positions)
			
			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(values clause flip-pos-list)))
		     
		     ((and (find posonlyfirst1 back-pos-list :test 'keim~equal)
			   (not (find posonlyfirst2 back-pos-list :test 'keim~equal)))
		      
		      ;; erste Literal wurde geflippt, zweites aber nicht!
		      ;; Justification muss angepasst werden -> andere Seiten uniziert in positions
		      
		      (let* ((new-positions (list (pos~list-position (append (butlast (pos~number-list pos1))
									   (if (equal (first (last (pos~number-list pos1))) 1)
									       (list 2)
									     (list 1))))
						  pos2))
			     (flip-pos-list (filter=update-pos-list-equality-factoring parent back-pos-list
										       posonlyfirst1 posonlyfirst2
										       remaining-position)))
			
			(setf (res~justification-positions just) new-positions)
			
			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(values clause flip-pos-list)))
		     
		     ((and (not (find posonlyfirst1 back-pos-list :test 'keim~equal))
			   (find posonlyfirst2 back-pos-list :test 'keim~equal))
		      
		      ;; erste Literal wurde nicht geflippt, aber zweites!
		      ;; Justification muss angepasst werden -> andere Seiten uniziert in positions
		      
		      (let* ((new-positions (list pos1
						  (pos~list-position (append (butlast (pos~number-list pos2))
									     (if (equal (first (last (pos~number-list pos2))) 1)
										 (list 2)
									       (list 1))))))
			     (flip-pos-list (filter=update-pos-list-equality-factoring parent back-pos-list
										       posonlyfirst1 posonlyfirst2
										       remaining-position)))
			
			(setf (res~justification-positions just) new-positions)
		      
			;; flippen der Literale
			(mapcar #'(lambda (pos)
				    (filter=flip-literal-at-position! clause pos))
				flip-pos-list)
			
			(values clause flip-pos-list)))))))
  (:method (clause (just res+paramodulation))
	   (let* ((mother (res~paramod-mother just))
		  (father (res~paramod-father just))
		  (full-mother-pos (res~paramod-mother-position just))
		  (mother-pos (pos~list-position (list (pos~first full-mother-pos))))
		  (father-pos (res~paramod-father-position just)))
	     
	     (multiple-value-bind
		 (mother-back-clause mother-back-pos-list)
		 (filter~remove-flip-steps! mother)
	       (multiple-value-bind
		   (father-back-clause father-back-pos-list)
		   (filter~remove-flip-steps! father)
		 
		 ;; ersetzten der parent-clauses durch back-clauses
		 (setf (res~justification-parents just) (list mother-back-clause father-back-clause))
		 
		 
		 (cond ((and (not (find mother-pos mother-back-pos-list :test 'keim~equal))
			     (not (find father-pos father-back-pos-list :test 'keim~equal)))
			
			(let* ((flip-pos-list (filter=update-pos-list-paramodulation mother mother-back-pos-list
										   father-back-pos-list mother-pos father-pos)))
			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)
			  
			 			    
			  (values clause flip-pos-list)))
		       ((and (not (find mother-pos mother-back-pos-list :test 'keim~equal))
			     (find father-pos father-back-pos-list :test 'keim~equal))
			
			(let* ((flip-pos-list (filter=update-pos-list-paramodulation mother mother-back-pos-list
										   father-back-pos-list mother-pos father-pos)))
			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)

			  ;; umdrehen der paramodulation direction
			  (filter=change-direction! just)
			  
			  (values clause flip-pos-list)))
		       ((and (find mother-pos mother-back-pos-list :test 'keim~equal)
			     (not (find father-pos father-back-pos-list :test 'keim~equal)))
			
			(let* ((flip-pos-list (filter=update-pos-list-paramodulation mother mother-back-pos-list
										   father-back-pos-list mother-pos father-pos
										   :add-mother-pos 't)))
			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)

			  ;; die mother-position wird gedreht
			  (filter=change-mother-position! just)

			  (values clause flip-pos-list)))
		       (t

			(let* ((flip-pos-list (filter=update-pos-list-paramodulation mother mother-back-pos-list
										   father-back-pos-list mother-pos father-pos
										   :add-mother-pos 't)))

			  
			  ;; flippen der Literale
			  (mapcar #'(lambda (pos)
				      (filter=flip-literal-at-position! clause pos))
				  flip-pos-list)
			  
			  ;; die mother-position wird gedreht
			  (filter=change-mother-position! just)
			  
			  ;; umdrehen der paramodulation direction
			  (filter=change-direction! just)
			  
			  (values clause flip-pos-list))))))))
  (:method (clause (just res+ur-resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (nucleus-rest-pos (first (res~resolution-positions just)))
		  (nucleus-rest-num (pos~first nucleus-rest-pos))
		  (back-clauses-and-positions (mapcar #'(lambda (clause)
							  (multiple-value-bind
							      (back-clause back-pos-list)
							      (filter~remove-flip-steps! clause)
							    (list back-clause back-pos-list)))
						      parents))
		  (back-clauses (mapcar #'first back-clauses-and-positions))
		  (back-nucleus (first back-clauses))
		  (back-electrons (rest back-clauses))
		  (back-position-lists (mapcar #'second back-clauses-and-positions))
		  (nucleus-back-pos-list (first back-position-lists))
		  (electrons-back-pos-lists (rest back-position-lists)))

	     ;; ersetzten der parent-clauses durch back-clauses
	     (setf (res~justification-parents just) back-clauses)
	     
	     (do* ((i 0 (+ i 1))
		   (rest-nucl-lits (cl~literals back-nucleus) (rest rest-nucl-lits))
		   (rest-electrons back-electrons)
		   (rest-electrons-back-pos-lists electrons-back-pos-lists))
		 ((null rest-nucl-lits)
		  nil)
	       (when (not (= i nucleus-rest-num))
		 (let* (;;(head-nucl-lit (first rest-nucl-lits))
			(head-lit-pos (pos~list-position (list i)))
			(according-electron (first rest-electrons))
			(according-electron-back-pos-list (first rest-electrons-back-pos-lists)))
		   
		   (setq rest-electrons (rest rest-electrons))
		   (setq rest-electrons-back-pos-lists (rest rest-electrons-back-pos-lists))
		   
		   (cond ((or (and (not (find head-lit-pos nucleus-back-pos-list :test 'keim~equal))
				   (null according-electron-back-pos-list))
			      (and (find head-lit-pos nucleus-back-pos-list :test 'keim~equal)
				   according-electron-back-pos-list))
			  nil)
			 (t
			  (filter=insert-flip-step-between-clauses! just according-electron (pos~list-position (list 0))))))))

	     (if (find nucleus-rest-pos nucleus-back-pos-list :test 'keim~equal)
		 (progn
		   (filter=flip-literal-at-position! clause (pos~list-position (list 0)))
		   (values clause (list (pos~list-position (list 0)))))
	       (values clause nil))))
  (:method (clause (just res+hyper-resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (back-clauses-and-positions (mapcar #'(lambda (clause)
							  (multiple-value-bind
							      (back-clause back-pos-list)
							      (filter~remove-flip-steps! clause)
							    (list back-clause back-pos-list)))
						      parents))
		  (back-clauses (mapcar #'first back-clauses-and-positions))
		  (back-nucleus (first back-clauses))
		  (back-electrons (rest back-clauses))
		  (back-position-lists (mapcar #'second back-clauses-and-positions))
		  (nucleus-back-pos-list (first back-position-lists))
		  (electrons-back-pos-lists (rest back-position-lists))
		  (positions (res~resolution-positions just))
		  (nucleus-literals (cl~literals back-nucleus))	  
		  (nucl-resolve-literals (if (typep just 'res+negative-hyper-resolution)
					     (remove-if-not #'lit~positive-p nucleus-literals)
					   (remove-if #'lit~positive-p nucleus-literals)))
		  (new-back-pos-list (filter=update-pos-list-hyper-resolution back-nucleus
									      back-electrons
									      nucleus-back-pos-list
									      electrons-back-pos-lists
									      positions
									      (if (typep just 'res+negative-hyper-resolution)
										  nil
										't))))
	     
	     ;; ersetzten der parent-clauses durch back-clauses
	     (setf (res~justification-parents just) back-clauses)
	     

	     ;; insert necessary flip steps
	     (do* ((rest-electrons back-electrons (rest rest-electrons))
		   (rest-elec-back-pos-lists electrons-back-pos-lists (rest rest-elec-back-pos-lists))
		   (rest-positions positions (rest rest-positions))
		   (rest-nucl-resolve-literals nucl-resolve-literals (rest rest-nucl-resolve-literals)))
		 ((null rest-electrons)
		  nil)
	       (let* ((head-electron (first rest-electrons))
		      (head-electron-back-pos-list (first rest-elec-back-pos-lists)) 
		      (head-elec-pos (first rest-positions))
		      (head-nucl-lit (first rest-nucl-resolve-literals))
		      (nucl-lit-pos (filter=position-of-literal head-nucl-lit nucleus-literals)))
		 (cond ((or (and (find head-elec-pos head-electron-back-pos-list :test 'keim~equal)
				 (find nucl-lit-pos nucleus-back-pos-list :test 'keim~equal))
			    (and (not (find head-elec-pos head-electron-back-pos-list :test 'keim~equal))
				 (not (find nucl-lit-pos nucleus-back-pos-list :test 'keim~equal))))
			nil)
		       (t
			(filter=insert-flip-step-between-clauses! just head-electron head-elec-pos)))))

	     ;; flippen der Literale
	     (mapcar #'(lambda (pos)
			 (filter=flip-literal-at-position! clause pos))
		     new-back-pos-list)
	     
	     (values clause
		     new-back-pos-list))))


(defun filter=update-pos-list-hyper-resolution (nucleus electrons nucleus-back-pos-list electrons-back-pos-lists positions
							electron-polarity)
  (multiple-value-bind
      (new-back-pos-list next-pos-number)
      (do* ((rest-nucl-lits (cl~literals nucleus) (rest rest-nucl-lits))
	    (i 0 (+ i 1))
	    (j 0)
	    (liste nil))
	  ((null rest-nucl-lits)
	   (values liste j))
	(let* ((head-lit (first rest-nucl-lits)))
	  (cond ((and (equal electron-polarity (lit~positive-p head-lit))
		      (find (pos~list-position (list i)) nucleus-back-pos-list :test 'keim~equal))
		 ;; literal is not resolved with a electron -> remaining in the new-clause
		 ;; and the literal is unflipped
		 (setq liste (cons (pos~list-position (list j)) liste))
		 (setq j (+ j 1)))
		((equal electron-polarity (lit~positive-p head-lit))
		 ;; literal is not resolved with a electron -> remaining in the new clause
		 ;; but was NOT unflipped
		 (setq j (+ j 1)))
		(t
		 nil))))
    (do* ((rest-electrons electrons (rest rest-electrons))
	  (rest-positions positions (rest rest-positions))
	  (rest-electron-back-pos-lists electrons-back-pos-lists (rest rest-electron-back-pos-lists))
	  (liste new-back-pos-list)
	  (current-pos next-pos-number))
	((null rest-electrons)
	 liste)
      (let* ((electron (first rest-electrons))
	     (position (first rest-positions))
	     (position-num (pos~first position))
	     (rest-electron-back-pos-list (remove position
						  (first rest-electron-back-pos-lists)
						  :test 'keim~equal)))
	(mapcar #'(lambda (back-pos)
		    (let* ((back-pos-num (pos~first back-pos)))
		      (cond ((< back-pos-num position-num)
			     (setq liste (cons (pos~list-position (list (+ back-pos-num current-pos))) liste)))
			    ((> back-pos-num position-num)
			     (setq liste (cons (pos~list-position (list (+ (- back-pos-num 1) current-pos))) liste))))))
		rest-electron-back-pos-list)
	(setq current-pos (+ current-pos (- (length (cl~literals electron)) 1)))))))


(defun filter=position-of-literal (literal list-of-literals)
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
	 nil)))) 


(defun filter=update-pos-list-equality-factoring (parent back-pos-list posf1 posf2 remaining-pos)

  ;; geflippt werden muessen
  ;; Wenn das Remaining-literal unter den geflippten ist, dann muss auch an posf1 geflippt werden
  ;; Wenn das nicht Remaining-Literal unter den geflippten ist, so muss es nicht mehr geflippt werden
  ;; Insbesondere muss an dem neuen neg= Literal nicht geflippt werden.
  ;; Fuer alle anderen gilt: Flips werden uebernommen!

  (let* ((posf1-number (pos~first posf1))
	 (posf2-number (pos~first posf2))
	 (remaining-pos-number (pos~first remaining-pos)))
  
    (apply #'append (mapcar #'(lambda (pos)
				(let* ((pos-number (pos~first pos)))
				  
				  (cond ((= pos-number remaining-pos-number)
					 ;; Wenn remaining-pos-number unter den geflippten ist
					 ;; -> es muss an nachher an posf1 geflippt werden
					 (list (pos~list-position (list posf1-number))))
					((or (= pos-number posf1-number)
					     (= pos-number posf2-number))
					 ;; Das Literal, das nicht uebrig bleibt wurde geflippt
					 ;; -> egal
					 nil)
					((< pos-number posf2-number)
					 ;; Andere geflippte Position < als posf2-number
					 ;; -> Position bleibt genau so erhalten
					 (list (pos~list-position (list pos-number))))
					((> pos-number posf2-number)
					 ;; Anderes Literal > posf2-number
					 ;; -> da durch Factoring ein Literal verschwindet, muss
					 ;;    Position um 1 vermindert werden
					 (list (pos~list-position (list (- pos-number 1))))))))
			    back-pos-list))))

					     


(defun filter=update-pos-list-paramodulation (mother mother-pos-list father-pos-list mother-pos father-pos &key (add-mother-pos nil))
  (declare (edited  "13-OCT-1997")
	   (authors Ameier)
	   (input   "A clause, two lists of positions and two positions.")
	   (effect  "None.")
	   (value   "A list of positions."))
  (let* ((mother-length (length (cl~literals mother)))
	 (mother-number-list (mapcar #'(lambda (pos)
					 (first (pos~number-list pos)))
				     mother-pos-list))
	 (father-number-list (mapcar #'(lambda (pos)
					 (first (pos~number-list pos)))
				     father-pos-list))
	 (par-mother-num (pos~first mother-pos))
	 (par-father-num (pos~first father-pos))
	 (updated-pos-list (apply 'append (append (mapcar #'(lambda (pos)
							      (cond ((or (< pos par-mother-num) (> pos par-mother-num))
								     (list (pos~list-position (list pos))))
								    ((= pos par-mother-num)
								     nil)))
							  mother-number-list)
						  (mapcar #'(lambda (pos)
							      (cond ((< pos par-father-num)
								     (list (pos~list-position (list (+ pos mother-length)))))
								    ((= pos par-father-num)
								     nil)
								    ((> pos par-father-num)
								     (list (pos~list-position (list (- (+ pos mother-length) 1)))))))
							  father-number-list)))))
    (if add-mother-pos
	(cons mother-pos updated-pos-list)
      updated-pos-list)))


(defun filter=update-pos-list-resolution (parent1 pos-list1 pos-list2 res-pos1 res-pos2)
  (declare (edited  "10-OCT-1997")
	   (authors Ameier)
	   (input   "A clause, two lists of positions and two positions.")
	   (effect  "None.")
	   (value   "A list of positions."))
  (let* ((length-cl1 (- (length (cl~literals parent1)) 1))
	 (number-list1 (mapcar #'(lambda (pos)
				   (first (pos~number-list pos)))
			       pos-list1))
	 (number-list2 (mapcar #'(lambda (pos)
				   (first (pos~number-list pos)))
			       pos-list2))
	 (res-number1 (pos~first res-pos1))
	 (res-number2 (pos~first res-pos2)))
    (apply 'append (append
		    (mapcar #'(lambda (pos)
				(cond ((< pos res-number1)
				       (list (pos~list-position (list pos))))
				      ((= pos res-number1)
				       nil)
				      ((> pos res-number1)
				       (list (pos~list-position (list (- pos 1)))))))
			    number-list1)
		    (mapcar #'(lambda (pos)
				(cond ((< pos res-number2)
				       (list (pos~list-position (list (+ pos length-cl1)))))
				      ((= pos res-number2)
				       nil)
				      ((> pos res-number2)
				       (list (pos~list-position (list (- (+ pos length-cl1) 1)))))))
			    number-list2)))))

    
(defun filter=update-pos-list-factoring (pos-list fac-pos1 fac-pos2 &key (add-fac-pos nil))
  (declare (edited  "10-OCT-1997")
	   (authors Ameier)
	   (input   "A list of positions and two positions and a flag add-fac-pos.")
	   (effect  "None.")
	   (value   "A list of positions."))
  (let* ((number-list (mapcar #'(lambda (pos)
				  (first (pos~number-list pos)))
			      pos-list))
	 (fac-num1 (pos~first fac-pos1))
	 (fac-num2 (pos~first fac-pos2))
	 (lesser-num (if (< fac-num1 fac-num2)
			 fac-num1
		       fac-num2))
	 (greater-num (if (< fac-num1 fac-num2)
			  fac-num2
			fac-num1))
	 (updated-pos-list
	  (apply 'append (mapcar #'(lambda (number)
				     (cond ((or (= number greater-num)
						(= number lesser-num))
					    nil)
					   ((< number greater-num)
					    (list (pos~list-position (list number))))
					   (t
					    (list (pos~list-position (list (- number 1)))))))
				 number-list))))
    (if add-fac-pos
	(cons (pos~list-position (list lesser-num))
	      updated-pos-list)
      updated-pos-list)))

	      
(defun filter=change-direction! (just)
  (declare (edited  "13-OCT-1997")
	   (authors Ameier)
	   (input   "A paramodulation justification.")
	   (effect  "The direction of the paramodulation is changed (LR -> RL and viceversa).")
	   (value   "Undefined."))
  (let* ((direction (res~paramod-direction just)))
    (if (equal direction 'LR)
	(setf (res~paramod-just-direction just) 'RL)
      (setf (res~paramod-just-direction just) 'LR))))


(defun filter=change-mother-position! (just)
  (declare (edited  "13-OCT-1997")
	   (authors Ameier)
	   (input   "A paramodulation justification.")
	   (effect  "The position of the mother clause is changed. The third number of the mother position"
		    "is changed from 1 to 2 and viceversa. (The first number signs the literal, the second"
		    "number - always 1 - signs the atom and the third number signs the arguments of the"
		    "topmost application: the equation.)")
	   (value   "Undefined."))
  (let* ((mother-position (res~paramod-mother-position just))
	 (father-position (res~paramod-father-position just))
	 (literal-pos (pos~first mother-position))
	 (mother-pos-cut-2 (pos~rest (pos~rest mother-position)))
	 (arg-pos (pos~first mother-pos-cut-2))
	 (new-arg-pos (if (= arg-pos 1)
			  2
			1)))
    (setf (res~justification-positions just)
	  (list (pos~add-front literal-pos
			       (pos~add-front 1
					      (pos~add-front new-arg-pos
							     (pos~rest mother-pos-cut-2))))
		father-position))))


(defun filter=insert-flip-step-between-clauses! (just parent-clause flip-pos)
  (declare (edited  "10-OCT-1997")
	   (authors Ameier)
	   (input   "A justification of an clause, one of its parent-clauses and a position"
		    "in that parent-clause.")
	   (effect  "A new clause is created by flipping the literal at the position in the"
		    "parent-clause. The parent-clause is replaced in the justification by this"
		    "new clause.")
	   (value   "Undefined."))
  (let* ((literal-at-pos (data~struct-at-position parent-clause flip-pos))
	 (atom-args (data~appl-arguments (lit~atom literal-at-pos))))

    (when (not (keim~equal (first atom-args) (second atom-args)))

      ;; andern falls (t=t) ist es unoetig zu flippen !!
      
      (let* ((already-flipped-clauses (keim~get parent-clause 'already-flipped-clauses))
	     (already-flipped-positions (keim~get parent-clause 'already-flipped-positions)))
	(if (and already-flipped-clauses (find flip-pos already-flipped-positions :test 'keim~equal))
	    
	    ;; parent-clause wurde bereits an position flip-pos geflippt -> nimm die dabi erzeugte Klausel
	    (let* ((new-clause (do* ((rest-clauses already-flipped-clauses (rest rest-clauses))
				     (rest-positions already-flipped-positions (rest rest-positions))
				     (flip-clause nil))
				   (flip-clause flip-clause)
				 (when (keim~equal flip-pos (first rest-positions))
				   (setq flip-clause (first rest-clauses))))))
	      ;; change the parents in just
	      (setf (res~justification-parents just) (substitute new-clause parent-clause (res~justification-parents just))))
	  
	  ;; erzeuge neue Flip-clausel fuer position flip-pos
	  (let* ((literals (cl~literals parent-clause))
		 (new-literals (data~copy literals :downto '(data+primitive)))
		 (flip-just (res~flip-create parent-clause flip-pos))
		 (new-clause (cl~create new-literals
					:justification flip-just)))
	    (filter=flip-literal-at-position! new-clause flip-pos)
	    
	    ;; change the parents in just
	    (setf (res~justification-parents just) (substitute new-clause parent-clause (res~justification-parents just)))
	    
	    ;; fuege neue flip-clausel und flip-pos zu den already-flipped-clauses und already-flipped-positions hinzu
	    (keim~put parent-clause 'already-flipped-clauses (cons new-clause already-flipped-clauses))
	    (keim~put parent-clause 'already-flipped-positions (cons flip-pos already-flipped-positions))))))))


(defun filter=flip-literal-at-position! (clause position)
  (declare (edited  "12-OCT-1997")
	   (authors Ameier)
	   (input   "A clause and a position.")
	   (effect  "The literal at the position in the clause is flipped if it is an equation."
		    "(otherwise error)")
	   (value   "Undefined."))
  (let* ((lit-to-flip (data~struct-at-position clause position))
	 (atom (lit~atom lit-to-flip)))
    
    ;; check wether the atom of the literal is an equation
    (when (or (not (data~appl-p atom))
	      (not (string-equal (keim~name (data~appl-function atom)) "=")))
      (error "Expecting an equation in filter=flip-literal-at-position!"))
    
    ;; flip the literal
    (setf (lit~atom lit-to-flip)
	  (term~appl-create (data~appl-function (lit~atom lit-to-flip))
			    (reverse (data~appl-arguments (lit~atom lit-to-flip)))))))







