;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; ana-ina.lisp; This file is part of the OMEGA system
;;
;; major updates: 4.10.1999
;; 
;;
;; Authors: Carsten Ullrich
;; email: cullrich@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; internal analogy

(in-package "OMEGA")


(mod~defmod ANAINA
            :uses (infer just keim node omega pds pdsc pdsn prob)
            :documentation "Internal Analogy"
            :exports (
                      
                      ana~ina
		      ))



(defun ana~ina (old-source-node target-node)
  (declare (edited  "09-JUL-1999")
	   (authors Cullrich)
	   (input   "Two nodes.")
	   (effect  "Proves the source node analogous to the proof of the target node.")
	   (value   "The new pds."))
  (omega~trace "Starting internal analogy.")
  (setf start-time (get-internal-run-time))
  (setf ana*processed-nodes nil)
  (setf ana*matched-nodes 0)
  (setf ana*applied-refs 0)
  (if (pdsn~open-node-p target-node)
      (let* ((source-node (if old-source-node old-source-node
			    (ana=ina-retrieve target-node
					      omega*current-proof-plan)
			    ))
             (goal-match ;(subst~create nil nil))
              (and (not (progn (omega~message "Now matching source and target node.") nil))
                   (ana~choose-match (ana~match (list source-node) (list target-node)))))
	     (source-hyps-labels
	      (ana=get-all-premises-of-nodes (list source-node) nil))
	     (source-hyps (mapcar #'pds~label2node source-hyps-labels))
	     (plan (ana=get-subproof source-hyps-labels))
	     (ana*internal-analogy t)
	     (ana*ina-steps (append (rest plan);; we mark the end of the source with a special
				    ;; symbol
				    (list 'ina-end)))
	     (target-hyps (pds~node-supports target-node
					     omega*current-proof-plan))
	     (node-table (ana~initialize-correspondance-table
			  (list (list source-hyps
				      target-hyps)
				(list (list source-node)
				      (list target-node)))))
	     )
	(omega~trace "Now starting internal analogy on target ~s with source ~s!"
		     target-node source-node)
	(ana=transfer-step omega*current-proof-plan
			   omega*current-proof-plan
			   (first plan)
			   (ana~matcher-subst goal-match)
			   node-table)
	(omega~trace "Internal analogy is completed!")
	(omega~message "Replay tooked ~F seconds." (/ (- (get-internal-run-time)
							 start-time)
						      internal-time-units-per-second))
	(omega~message "Matched ~a nodes." ana*matched-nodes)
	(omega~message "Matching attemps: ~a~T Applied methods: ~a~%Open nodes:
~a~%Applied refs: ~a."
		       plan*matching-attempts
		       plan*number-of-applied-methods (length (pds~open-nodes
							       omega*current-proof-plan))
		       ana*applied-refs
		       )
	(omega~message "%Semantic :~a~T Table:~a" (not ana*no-semantic-choice) ana*use-node-table)
	(unless (pds~open-nodes omega*current-proof-plan)
	  (omega~trace "All nodes were closed.")))
    (omega~message "~s is not open!" target-node)))

(defun ana=ina-retrieve (target-node pds)
  (omega~message "Looking for a source node...")
  (let ((result (some
		 #'(lambda (step)
		     (let* ((just (pdsc~an-just step))
			    (method (ana~just-method just)))
		       (when (and (and method (meth~exist-conclusions method) ;;checking for bw-methods
				       (not (eq (keim~name method) 'ExistsE-m-a)))
				  (ana~method-is-matching method
							  (list target-node)
							  (pds~node-supports target-node
									     pds)
							  (when (meth~parameters method)
							    (pdsj~parameters just))
							  pds))
			 (pdsc~an-node step))))
		 (pds~plan-steps pds))))
    (if result
	(progn
	  (omega~message "Source node found: ~s" result)
	  result)
      (progn
	(omega~message "No source node found... internal analogy not possible.")
	nil))))

				    


(defun ana=get-subproof (hyps)
  (omega~message "Now retrieving source plan.")
  (remove-if-not #'(lambda (step)
		     (member (keim~name (pdsc~an-node step))
			     hyps))
		 (pds~plan-steps omega*current-proof-plan)))

(defun ana=get-all-premises-of-nodes (nodes result)
  ;; returns all following premises of the nodes in nodes list
  (if nodes
      (if (member (first nodes) result)
	  (ana=get-all-premises-of-nodes (rest nodes)
					 result)
	(ana=get-all-premises-of-nodes (append (pdsn~just-premises (first nodes))
					       (rest nodes))
				       (cons (keim~name (first nodes)) result)))
    result))

;(defun ana=get-premises (nodes result)
;  (if nodes
;      (ana=get-premises (append (pdsn~just-premises (first nodes))
;                                (rest nodes))
;                        (cons (keim~name (first nodes)) result))
;    result))


#|
(defun ana=union-of-hyps-h (nodes)
  (when nodes
    (union (pdsn~hyps (first nodes))
	   (ana=union-of-hyps-h (rest nodes)))))

(defun ana=union-of-prems-h (nodes)
  (when nodes
    (let* ((node (first nodes))
	   (just (node~justification node))
	   (infer (just~method just))
	   )
      (if (or (infer~dummy-p infer)
	      (pdsn~open-node-p node)
	      (pdsn~hypothesis-node-p node))
	  (ana=union-of-prems-h (rest nodes))
	(union (just~premises just)
	       (ana=union-of-prems-h (rest nodes)))))))


(defun ana=set-difference-h (set1 set2)
  (declare (edited  "28-JAN-1999")
	   (authors Sorge)
	   (input   "Two list representing sets.")
	   (effect  "None.")
	   (value   "A list containing the members of set1 which are not in set2."
		    "The order of set1 is respected."))
  (remove-if #'(lambda (x) (find x set2)) set1))
|#

#|
(defvar ana*ina-flag nil "Should we use internal analogy or not?")

(defun ana~check-for-internal-analogy (new-opens old-opens)
  (let ((not-closed)
	(used-nodes))
    (dolist (node new-opens not-closed)
      (if (member (keim~name node) used-nodes)
	  ;; we do not want to close this not by ina if it is used to closed another node!
	  (push node not-closed)
	(let ((match-result (ana~match (append
					;;we remove the actual node(we don't want to match it
					;;to itself)
					(remove-if #'(lambda (n)
						       (eq (keim~name n)
							   (keim~name node)))
						   new-opens)
					old-opens)
				       (list node)
				       nil
				       '(0 0 99)) ;;ist das zu viel? vielleicht
						;;Individuenvars zaehlen?
			    ))
	  (if match-result
	      (progn (ana~close-node node (first match-result))
		     (push (keim~name (ana~matcher-source-node (first match-result)))
			   used-nodes))
	    (push node not-closed)))))
    (reverse not-closed))
  )

(defun ana~close-node (node matcher)
  (progn
    (omega~trace "INA: closing node ~s with ~s by internal analogy!" node
		 (ana~matcher-source-node matcher))
    (setf (node~justification node)
	  (pdsj~closed-just-create (meth~find-method 'foralli*-m);infer+analogy 
				   (list (ana~matcher-source-node matcher))
				   nil
				   "unexpanded"
				   nil
				   '("EXISTENT" "NONEXISTENT")
					;matcher
				   ))))

;(infer~defanalogy "analogy")
|#
