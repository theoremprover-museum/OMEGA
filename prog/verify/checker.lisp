;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; checker.lisp; This file is part of the OMEGA system
;;
;; 
;; 
;;
;; Author:  Carsten Ullrich
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
 
(in-package "OMEGA")

(defgeneric check~check (thing not-expand)
  (declare (edited  "12-FEB-1998")
	   (authors Cullrich)
	   (input   "something to check (proof, list of nodes...). not-expand is a list of
tactics that will not be expanded.")
	   (effect  "none")
	   (value   "a list of two elements: true or nil depending on whether the justifications
were correct and the list of uncorrect lines. The given tactics are not expanded butc
hecked with tac~apply, all other will be expanded and then checked. If not-expand is true, no method will be expanded."))

  ;; first the problem is expanded then it is checked

  (:method ((thing prob+proof) not-expand)
	   (let ((nodes-to-unexpand (check=expand-nodes-in-proof
				     (when not-expand
				       (mapcar #'infer~find-method not-expand))))
		 (result (progn
			   (omega~message "Checking nodes.")
			    (check=check (prob~proof-steps thing)
				      )))
		 )
	     (if (first result)
		 (progn (if nodes-to-unexpand (check=unexpand-nodes nodes-to-unexpand))
			result)
	       result)
	     ))
  (:method ((thing list) not-expand)
	   (omega~message "Checking nodes.")
	   (check=check (mapcar #'pds~label2node thing))
	   )
)    

(defgeneric check=check (thing)
  (:method ((thing view+pds-node))
	   (if (check=single-node thing
				  (pdsn~just-method thing)
				  )
	       (progn
		 (omega~message "Node ~S has a correct justification." thing)
		 (list t ())
		 )
	     (progn
	       (omega~warn "Node ~S with the formula ~S has an uncorrect or unknown
justification!~%" thing (node~formula thing))
	       (list nil (list thing)))))

  (:method ((thing view+pds-schematic-node))
	   (if (check=single-node thing
				  (pdsn~just-method thing)
				  )
	       (progn
		 (omega~message "Node ~S has a correct justification." thing)
		 (list t ())
		 )
	     (progn
	       (omega~warn "Node ~S with the formula ~S has an uncorrect or unknown
justification!~%" thing (node~formula thing))
	       (list nil (list thing)))))

  (:method ((thing list))
	   (if (not thing) (list t ()) ;empty-list is correct
	     (let ((result (check=check (first thing)
					))
		   (result-rest (check=check (cdr thing)
					     ))
		   )
	       (list (and (first result) (first result-rest))
		     (append (second result) (second result-rest))))))
  )


(defgeneric check=single-node (node inference)
  (:method (node (inference infer+dummy))
	   (declare (ignore node))
	   t)
	   
  (:method (node (inference infer+rule))
	   (let* ((justification (node~justification node))
		  (outline-mapping (make-list (1+ (length
						   (just~premises
						    justification)))
					      :initial-element 'closed))
		  (application (infer~outline-pattern2application inference outline-mapping)))
	     (if application
		 (rule~apply
		  application
		  (append (list node) (just~premises justification))
		  (pdsj~parameters justification)
		  t :purpose :test)
	   (progn (omega~warn "The application for the rule ~S and the outline-mapping
                             ~S is not defined, no checking possible.~%" inference outline-mapping)
		  nil))))
  (:method (node (inference infer+tactic))
	   (let* ((justification (node~justification node))
		  (outline (oc~build-outline node (pdsj~outline-pattern justification)
					     (just~premises justification)))
		  (outline-mapping (make-list (length outline)
					      :initial-element 'closed))
		  (application (infer~outline-pattern2application inference outline-mapping)))
	     (if application
		 (tac~apply
		  application
		  outline
		  (pdsj~parameters justification)
		  t :purpose :test)

	       (progn (omega~warn "The application for the tactic ~S and the outline-mapping
                             ~S is not defined, no checking possible.~%" inference outline-mapping)
		      nil)))
	   )

  (:method (node (inference infer+wild-tactic))
	   (progn (omega~warn "The application of the wild-tactic ~S is not checked~%" inference)
		  nil))
  
  (:method (node (inference infer+method))
	   (omega~warn "~%Don't know yet how to handle the interference for this node: ~S" node)
	   nil)

  (:method (node (inference infer+black-box))
	   (omega~warn "Don't know yet how to handle the interference for this node: ~S" node)
	   nil)
  )



;;;;;;;;;;;;
;;; expanding functions




(defun check=expand-nodes-in-proof (not-expand)
  ;; expands some nodes and returns them
  (let ((nodes-to-expand (check=nodes-to-expand not-expand)))
    ;; es werden zweimal die zu expandierenden Knoten bestimmt, wird aber benoetig um
    ;; nachher wieder einzufalten
    (omega~message "~%Expanding nodes......")
    (check=expand-nodes not-expand)
    nodes-to-expand))

(defun check=expand-nodes (not-expand)
  (let ((nodes (check=nodes-to-expand not-expand)))
    (when nodes
      (oc=expand-node* nodes)
      (check=expand-nodes not-expand))))

(defun check=nodes-to-expand (not-expand)
  ;; finds the nodes to expand (almost like oc=default-nodes-to-expand)
  (if (and not-expand (not (listp not-expand))) ;; are there nodes that should not be expanded?
      ()
    (let ((ng-nodes (remove-if #'(lambda (n) (pdsj~grounded-p (node~justification n)))
			       (remove-if #'pdsn~open-node-p
					  (prob~proof-steps omega*current-proof-plan)))))
      (remove-if #'(lambda (n) (member (node~just-method n)
					   not-expand))
		 ng-nodes)
      )))
  

(defun check=unexpand-nodes (nodes)
  (progn
        (omega~message "Unexpanding nodes......")
	(mapcar #'oc=fully-unexpand-node nodes))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Command functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc=check (not-expand)
  (when (first (check~check omega*current-proof-plan not-expand))
    (omega~message "~%Well done, the proof is correct.")))


(defun cc=check-nodes (nodes)
  (when (first (check~check nodes ()))
    (omega~message "~%Well done, the nodes are correct.")))

