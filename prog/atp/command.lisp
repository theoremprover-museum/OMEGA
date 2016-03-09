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

;; REMARK: ALL COMMANDS ARE NOW IN THE FILE: .../omega-3/commands/extern.com !!!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bliksem:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-bliksem-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-bliksem-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'bliksem))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-bliksem-and-untested-or-unexpanded
	    (first nodes-justified-by-bliksem-and-untested-or-unexpanded)
	  (com~unspecified))))))

(defun oc=command-call-bliksem-on-node (conclusion
					output-directory
					ressource
					expand
					command-string
					sspu-style
					indirect-proof
					integral-formulas
					maximal-depth
					tnd
					avoid-doubeling
					lemmas)
  
  (th~require-completely 'base)
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if bliksem is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))
	   
	   ;; justify the conclusion by bliksem
	   (infer~compute-outline (infer~find-method 'bliksem) (cons conclusion supports) (list nil))

	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)
	   ))
	
	((not (eq (infer~find-method 'bliksem) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method bliksem." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      output-directory
			      expand
			      ressource
			      (list command-string)
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EQP:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-eqp-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-eqp-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'eqp))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-eqp-and-untested-or-unexpanded
	    (first nodes-justified-by-eqp-and-untested-or-unexpanded)
	  (com~unspecified))))))

(defun oc=command-call-eqp-on-node (conclusion
				    output-directory
				    ressource
				    expand
				    command-string
				    sspu-style
				    indirect-proof
				    integral-formulas
				    maximal-depth
				    tnd
				    avoid-doubeling
				    lemmas)
  
  (th~require-completely 'base)
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if eqp is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))

	   ;; justify the conclusion by eqp
	   (infer~compute-outline (infer~find-method 'eqp) (cons conclusion supports) (list nil))
	   
	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)
	   ))
	((not (eq (infer~find-method 'eqp) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method eqp." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      output-directory
			      expand
			      ressource
			      (list command-string)
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WALDMEISTER:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-wald-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-wald-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'waldmeister))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-wald-and-untested-or-unexpanded
	    (first nodes-justified-by-wald-and-untested-or-unexpanded)
	  (com~unspecified))))))

(defun oc=command-call-wald-on-node (conclusion
				     output-directory
				     ressource
				     expand
				     command-string
				     sspu-style
				     indirect-proof
				     integral-formulas
				     maximal-depth
				     tnd
				     avoid-doubeling
				     lemmas)
  
  (th~require-completely 'base)
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if eqp is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))
	   
	   ;; justify the conclusion by waldmeister
	   (infer~compute-outline (infer~find-method 'waldmeister) (cons conclusion supports) (list nil))

	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)))
	((not (eq (infer~find-method 'waldmeister) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method WALDMEISTER." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      output-directory
			      expand
			      ressource
			      (list command-string)
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Satchmo:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-satchmo-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-satchmo-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'satchmo))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-satchmo-and-untested-or-unexpanded
	    (first nodes-justified-by-satchmo-and-untested-or-unexpanded)
	  (com~unspecified))))))

(defun oc=command-call-satchmo-on-node (conclusion
					output-directory
					ressource)
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if satchmo is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion)))
	   
	   ;; justify the conclusion by satchmo
	   (infer~compute-outline (infer~find-method 'satchmo) (cons conclusion supports) (list nil))))
	((not (eq (infer~find-method 'satchmo) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method SATCHMO." conclusion)))
  
  (atp~call-satchmo conclusion
		    output-directory
		    ressource))
		    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTTER:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-otter-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-otter-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'otter))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-otter-and-untested-or-unexpanded
	    (first nodes-justified-by-otter-and-untested-or-unexpanded)
	  (com~unspecified))))))


(defun oc=command-call-otter-on-node (conclusion
				      output-directory
				      mode
				      expand
				      proof-object
				      user-flag-string
				      user-weight-string
				      ressource
				      sspu-style
				      indirect-proof
				      integral-formulas
				      maximal-depth
				      tnd
				      avoid-doubeling
				      lemmas)

  (th~require-completely 'base)

  (cond ((pdsn~open-node-p conclusion)

	 ;; happens if otter is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))
	   
	   ;; justify the conclusion by otter
	   (infer~compute-outline (infer~find-method 'otter) (cons conclusion supports) (list nil))

	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   ;; AM (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   ;; AM (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)
	   ))
	((not (eq (infer~find-method 'otter) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method OTTER." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      output-directory
			      expand
			      ressource
			      (list mode
				    proof-object
				    user-flag-string
				    user-weight-string)
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPASS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-spass-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-otter-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'spass))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-otter-and-untested-or-unexpanded
	    (first nodes-justified-by-otter-and-untested-or-unexpanded)
	  (com~unspecified))))))


(defun oc=command-call-spass-on-node (conclusion
				      output-directory
				      expand
				      auto-mode
				      splitting-level
				      ressource
				      sspu-style
				      indirect-proof
				      integral-formulas
				      maximal-depth
				      tnd
				      avoid-doubeling
				      lemmas)

  (th~require-completely 'base)

  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if otter is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))
	   
	   ;; justify the conclusion by spass
	   (infer~compute-outline (infer~find-method 'spass) (cons conclusion supports) (list nil))

	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)
	   ))	
	((not (eq (infer~find-method 'spass) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method SPASS." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      output-directory
			      expand
			      ressource
			      (list auto-mode
				    splitting-level)
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-protein-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-protein-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'protein))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-protein-and-untested-or-unexpanded
	    (first nodes-justified-by-protein-and-untested-or-unexpanded)
	  (com~unspecified))))))

(defun oc=command-call-protein-on-node (conclusion
					dir
					expand
					ressource
					sspu-style
					indirect-proof
					integral-formulas
					maximal-depth
					tnd
					avoid-doubeling
					lemmas)
  
  (th~require-completely 'base)  
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if protein is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))
	   
	   ;; justify the conclusion by PROTEIN
	   (infer~compute-outline (infer~find-method 'protein) (cons conclusion supports) (list nil))

	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)
	   ))
	((not (eq (infer~find-method 'protein) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method PROTEIN." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      dir
			      expand
			      ressource
			      nil
			      
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PL-ATP:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun oc=default-current-plan-or-untested-or-unexpanded-pl-atp-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-pl-atp-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (eq (just~method (node~justification node)) (infer~find-method 'pl-atp))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-pl-atp-and-untested-or-unexpanded
	    (first nodes-justified-by-pl-atp-and-untested-or-unexpanded)
	  (com~unspecified))))))


(defun oc=command-call-pl-atp-on-node (conclusion
				       output-directory
				       mode
				       expand
				       proof-object
				       user-flag-string
				       user-weight-string
				       ressource
				       sspu-style
				       indirect-proof
				       integral-formulas
				       maximal-depth
				       tnd
				       avoid-doubeling
				       lemmas)
  
  (th~require-completely 'base)

  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if otter is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))
	   
	   ;; justify the conclusion by pl-atp
	   (infer~compute-outline (infer~find-method 'pl-atp) (cons conclusion supports) (list nil))

	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)
	   ))
	((not (eq (infer~find-method 'pl-atp) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method PL-ATP." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      output-directory
			      expand
			      ressource
			      (list mode
				    proof-object
				    user-flag-string
				    user-weight-string)
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trans RES PROOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=command-transform (resolution-proof prover where sspu-style indirect-proof integral-formulas
					      maximal-depth tnd avoid-doubeling lemmas)
  (th~require-completely 'base)
  
  (if (not (eq where 'alone))
      (let* ((new-pds (pds~find-proof-plan where)))
	(when (null new-pds)
	  (error "PDS ~A does not exist." where))

	(setq omega*current-proof-plan new-pds)
	
	(let* ((conclusion-node (pds~label2node (keim~name (res~proof-conclusion resolution-proof))))
	       (assumption-nodes (mapcar #'pds~label2node
					 (mapcar #'keim~name (res~proof-assumptions resolution-proof)))))

	  ;; compare whether the (names) of the conclusion and the assumptions of the resolution proof are existing
	  ;; in the pds
       	  
	  (when (member nil (cons conclusion-node assumption-nodes))
	    (error "The resolution proof ~A is not expandable in the PDS ~A"
		   (keim~name resolution-proof)
		   (keim~name new-pds)))
	  
	  ;; guarant, that the conclusion node in the pds has a OTTER/SPASS -justification with correct premises
	  
	  (let* ((atp-method (infer~find-method prover))
		 (atp-just (first (remove-if-not #'(lambda (just)
						     (eq (just~method just) atp-method))
						 (pdsn~all-justs conclusion-node)))))
	    
	    
	    (cond ((pdsn~open-node-p conclusion-node)
		   
		   ;; justify the conclusion by otter
		   (infer~compute-outline (infer~find-method prover) (cons conclusion-node assumption-nodes) (list nil)))
;;                   (setf (pds~open-nodes omega*current-proof-plan)
;;                         (remove conclusion-node (pds~open-nodes omega*current-proof-plan)))
;;                   
;;                   ;; the assumptions and the conclusion get this otter as reason
;;                   (let* ((atp-reason (pds~change-last-plan-step! conclusion-node)))
;;                     (mapcar #'(lambda (node)
;;                                 (pdsn~insert-reason! node atp-reason))
;;                             (cons conclusion-node assumption-nodes))))
		  (atp-just
		   (when (atptop~remove-list assumption-nodes (just~premises atp-just))
		     (error "The justification given by ~A for PDS node ~A contains the premises ~A that are not assumptions of the resolution proof ~A"
			    prover
			    (keim~name (res~proof-conclusion resolution-proof))
			    (otter=remove-list assumption-nodes (just~premises atp-just))
			    (keim~name resolution-proof))))
		  (t
		   (error "The conclusion ~A of the resolution proof ~A is within PDS ~A neither open nor justified by ~A."
			  prover
			  (keim~name (res~proof-conclusion resolution-proof))
			  (keim~name resolution-proof)
			  (keim~name omega*current-proof-plan))))))
	
	
	(setf (res~proof-upper-object resolution-proof) omega*current-proof-plan)       
	(atp~expand-resolution-proof-in-its-upper-object resolution-proof
							 prover
							 :sspu-style (case sspu-style
								       (direct 'dir)
								       (compact 'sspu)
								       (auto 'aut))
							 :indirect-proof indirect-proof
							 :maximal-depth maximal-depth
							 :integral-formulas integral-formulas
							 :reach-sspu-style (if tnd 'case 'smallest)
							 :avoid-doubeling avoid-doubeling
							 :lemmas lemmas))
    (progn
      (res2nd~transform resolution-proof
			:sspu-style (case sspu-style
				      (direct 'dir)
				      (compact 'sspu)
				      (auto 'aut))
			:indirect-proof indirect-proof
			:maximal-depth maximal-depth
			:integral-formulas integral-formulas
			:reach-sspu-style (if tnd 'case 'smallest)
			:avoid-doubeling avoid-doubeling
			:lemmas lemmas)
      (atptop~construct-and-add-problem! resolution-proof omega*current-proof-plan))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use otter out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun oc=use-otter-proof-on-node (conclusion
				   otter-out-file
				   out-style
				   sspu-style
				   indirect-proof
				   integral-formulas
				   maximal-depth
				   tnd
				   avoid-doubeling
				   lemmas)
  
  (th~require-completely 'base)
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if otter is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion)))
	   
	   ;; justify the conclusion by otter
	   (infer~compute-outline (infer~find-method 'otter) (cons conclusion supports) (list nil))))


	;;           (setf (pds~open-nodes omega*current-proof-plan) (remove conclusion (pds~open-nodes omega*current-proof-plan)))
	;;           
	;;           ;; the supports and the conclusion get this otter as reason
	;;           (let* ((otter-reason (pds~change-last-plan-step! conclusion)))
	;;             (mapcar #'(lambda (node)
	;;                         (pdsn~insert-reason! node otter-reason))
	;;                     (cons conclusion supports)))))
	((not (eq (infer~find-method 'otter) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method OTTER." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      'use-otter-out
			      'expand 
			      0
			      (list otter-out-file
				    out-style)
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call concurrent atp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=call-concurrent-atp-fo (node &optional (tps-tactic "MS88") (tps-mode nil))
  (let* ((loui-string (atp~compute-loui-string-fo node tps-tactic tps-mode omega*current-proof-plan))
	 (method (format nil "callConcurrentAtp(~a ~a)" (parse~string (keim~name node)) loui-string)))
    (socket~write method :inout)))

(defun oc=call-concurrent-atp (node &optional (tps-tactic "MS88") (tps-mode nil))
  (let* ((loui-string (atp~compute-loui-string node tps-tactic tps-mode omega*current-proof-plan))
	 (method (format nil "callConcurrentAtp(~a ~a)" (parse~string (keim~name node)) loui-string)))
    (socket~write method :inout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert atp out-string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand receive-atp-out
  (argnames node type)
  (argtypes ndline symbol)
  (arghelps "Node to add qtp-out"
	    "ATP-type")
  (frag-cats extern)
  (function atp~interpret-string-from-loui)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)
	     ""
	     ))
  (log-p T) 
  (help ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choose one resolution proof to transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=choose-problem-to-transform (node)
  (let* ((atp-problems (keim~get node 'atp-problems))
	 (complete-problems (remove-if-not #'atpprb~complete-p atp-problems))
	 (stringi (format nil "popupProofs(\"~A\" [" (keim~name node))))
    (mapcar #'(lambda (comp-prob)
		(let* ((type (atpprb~problem-type comp-prob)))
		  (setq stringi (format nil "~A \"~A\"" stringi (string-upcase (string type))))))
	    complete-problems)

    (setq stringi (format nil "~A])" stringi))

    (socket~write stringi :inout)))

(com~defcommand transform-problem-of-type
  (argnames node type)
  (argtypes ndline symbol)
  (arghelps "Node to choose a problem from"
	    "Type of problem")
  (frag-cats extern)
  (function atp~choose-atp-problem-to-transform)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)	     
	     'otter
	     ))
  (log-p T) 
  (help ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kleines Commando um interactivitaet an und aus zu schalten 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand interactivity
  (argnames flag)
  (argtypes boolean)
  (arghelps "interactivity is allowed")
  (frag-cats extern)
  (function oc=set-interactivity!)
  (defaults (nil
	     ))
  (log-p T) 
  (help ""))


(defun oc=set-interactivity! (flag)
  (setf atptop*interactivity-allowed flag))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FO ATP allgemein
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun oc=default-current-plan-or-untested-or-unexpanded-foatp-node ()
  (let* ((default-current-plan-node (oc~default-current-planline)))
    (if (not (listp default-current-plan-node))
	default-current-plan-node
      (let* ((nodes-justified-by-foatp-and-untested-or-unexpanded
	      (remove-if-not #'(lambda (node)
				 (let* ((just (node~justification node))
					(method (just~method just))
					(status (pdsj~status just)))
				   (and (or (eq (just~method (node~justification node)) (infer~find-method 'protein))
					    (eq (just~method (node~justification node)) (infer~find-method 'otter))
					    (eq (just~method (node~justification node)) (infer~find-method 'eqp))
					    (eq (just~method (node~justification node)) (infer~find-method 'waldmeister))
					    (eq (just~method (node~justification node)) (infer~find-method 'spass))
					    (eq (just~method (node~justification node)) (infer~find-method 'bliksem)))
					(or (string= status "unexpanded")
					    (string= status "untested")))))
			     (prob~proof-steps omega*current-proof-plan))))
	(if nodes-justified-by-foatp-and-untested-or-unexpanded
	    (first nodes-justified-by-foatp-and-untested-or-unexpanded)
	  (com~unspecified))))))

(defun oc=command-call-foatp-on-node (conclusion
				      atp-symbol
				      dir
				      expand
				      ressource
				      sspu-style
				      indirect-proof
				      integral-formulas
				      maximal-depth
				      tnd
				      avoid-doubeling
				      lemmas)
  
  (th~require-completely 'base)  
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if ATP is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion)))
	   
	   ;; justify the conclusion by ATP
	   (infer~compute-outline (infer~find-method atp-symbol) (cons conclusion supports) (list nil))))
	
	((not (or (eq (infer~find-method 'protein) (just~method (node~justification conclusion)))
		  (eq (infer~find-method 'otter) (just~method (node~justification conclusion)))
		  (eq (infer~find-method 'spass) (just~method (node~justification conclusion)))
		  (eq (infer~find-method 'bliksem) (just~method (node~justification conclusion)))
		  (eq (infer~find-method 'eqp) (just~method (node~justification conclusion)))
		  (eq (infer~find-method 'waldmeister) (just~method (node~justification conclusion)))
		  ))
	 (error "The node ~A should be a open node or should have a justification by a ATP method." conclusion)))
  
  (atp~call-res-based-prover conclusion
			      dir
			      expand
			      ressource
			      ;; Hier kommen die spezifischen Settings fuer die ATP's
			      (cond ((string-equal atp-symbol 'protein)
				     nil)
				    ((string-equal atp-symbol 'waldmeister)
				     (list (string "")))
				    ((string-equal atp-symbol 'eqp)
				     (list (string "")))
				    ((string-equal atp-symbol 'bliksem)
				     (list (string "")))
				    ((string-equal atp-symbol 'otter)
				     (list 'auto 't (string "") (string "")) ;; Auto mode on, proof object on, leere Strings
				     )
				    ((string-equal atp-symbol 'spass)
				     (list 't 0) ;; Auto mode on and Splitting Level 0
				     ))
			      
			      :sspu-style (case sspu-style
					    (direct 'dir)
					    (compact 'sspu)
					    (auto 'aut))
			      :indirect-proof indirect-proof
			      :maximal-depth maximal-depth
			      :integral-formulas integral-formulas
			      :reach-sspu-style (if tnd 'case 'smallest)
			      :avoid-doubeling avoid-doubeling
			      :lemmas lemmas))


;; -------------------------------- READING THE OUTPUT OF AN ATP



(defun oc=read-and-transform-atp-output-node (conclusion
					      out-file
					      sspu-style
					      indirect-proof
					      integral-formulas
					      maximal-depth
					      tnd
					      avoid-doubeling
					      lemmas)
  
  (th~require-completely 'base)

  (let* ((atp-sign (atptop~detect-atp out-file)))
  
    (cond ((pdsn~open-node-p conclusion)
	   ;; happens if ATP is directly called on the open node of a pds
	   (let* ((supports (pds~node-supports conclusion)))
	   
	     ;; justify the conclusion by atp-sign
	     (infer~compute-outline (infer~find-method atp-sign) (cons conclusion supports) (list nil))))
	  
	  ((not (eq (infer~find-method atp-sign) (just~method (node~justification conclusion))))
	   (error "The node ~A should be a open node or should have a justification by method ~A" conclusion atp-sign)))
    
    (atp~read-and-transform-proof conclusion
				  out-file
				  atp-sign
				  :sspu-style (case sspu-style
						(direct 'dir)
						(compact 'sspu)
						(auto 'aut))
				  :indirect-proof indirect-proof
				  :maximal-depth maximal-depth
				  :integral-formulas integral-formulas
				  :reach-sspu-style (if tnd 'case 'smallest)
				  :avoid-doubeling avoid-doubeling
				  :lemmas lemmas)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  ALL IN ONE COMMANDOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand call-fo-atps-on-first-node
		(argnames parse-all)
		(argtypes boolean)
		(arghelps "Should all ATP outputs be parsed?")
		(frag-cats extern)
		(function oc=call-fo-atps-on-first-node)
		(defaults (nil))
		(log-p T)
		(help "Seeks in the current pds for a open goal and calls the concurrent ATPs on this node."))

(com~defcommand transform-and-save-on-first-node
		(argnames transform expand-assertions output-file)
		(argtypes boolean boolean pathname)
		(arghelps "Flag to sign whether one of the found proofs should be transformed."
			  "Should the assertion applications be expanded."
			  "Name of the output-File.")
		(frag-cats extern)
		(function oc=transform-and-save-proof-on-first-node)
		(defaults ('t nil "~/test-out.pds"))
		(log-p T)
		(help "Seeks in the current pds for a open goal takes an ATP proof on this nodes and translates and saves the proof."))

(com~defcommand expand-all-assertions
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern)
		(function oc=expand-all-assertion-nodes)
		(defaults )
		(log-p T)
		(help "Expands all nodes which are justified by an assertion application."))

(defun oc=call-fo-atps-on-first-node (parse-all)
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan))))
    
    (setf atptop*parse-all parse-all)
    
    ;; Call the FO ATPS concurrently 
    (oc=call-concurrent-atp-fo open-node)))

(defun oc=transform-and-save-proof-on-first-node (transform expand-assertions out-file)
  (let* ((open-node (first (pds~open-nodes omega*current-proof-plan)))
	 (atp-problems (keim~get open-node 'atp-problems))
	 (complete-problems (remove-if-not #'atpprb~complete-p atp-problems))
	 (succeeded-provers-problems (remove-if-not #'atpprb~prover-succeeded-p atp-problems))
	 (types-of-complete-problems (mapcar #'atpprb~problem-type complete-problems))
	 (types-of-succeeded-problems (mapcar #'atpprb~problem-type succeeded-provers-problems)))

    (when (and transform types-of-complete-problems)
      (atp~choose-atp-problem-to-transform open-node (first types-of-complete-problems)))

    (when (and transform types-of-complete-problems expand-assertions)
      (oc=expand-all-assertion-nodes))
    
    (sys~handler-case
     (with-open-file (out out-file
			  :direction :output 
			  :if-exists :supersede
			  :if-does-not-exist :create)
		     (format out ";; OUTPUT CREATED BY TRAMP")
		     (format out "~%;; The following ATPs succeeded in proving the problem in the given time:")
		     (mapcar #'(lambda (type)
				 (format out " ~A" type))
			     types-of-succeeded-problems)
		     (format out "~%;; TRAMP read the proofs of:")
		     (mapcar #'(lambda (type)
				 (format out " ~A" type))
			     types-of-complete-problems)
		     (cond ((and transform types-of-complete-problems) 
			    (format out "~%~%;; TRAMP transformed the output of ~A." (first types-of-complete-problems))
			    (format out "~%;; This resulted in the following ND-Proof at Assertion Level:~%")
			    (post~print omega*current-proof-plan out)
			    )
			   (transform
			    (format out "~%;; Sorry, no transformable proof available!"))
			   (t
			    (format out "~%;; No transformation was demanded."))))
     (file-error (c) (omega~error c)))))

(defun oc=expand-all-assertion-nodes ()
  (let* ((all-nodes (prob~proof-steps omega*current-proof-plan))
	 (all-assertion-nodes (remove-if-not #'(lambda (node)
						 (let* ((just (node~justification node))
							(method (just~method just)))
						   (string-equal 'assertion (keim~name method))))
					     all-nodes)))

    (mapcar #'oc=expand-node all-assertion-nodes)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; transform TSTP Proof
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun oc=transform-tstp-proof (conclusion
				out-style
				sspu-style
				indirect-proof
				integral-formulas
				maximal-depth
				tnd
				avoid-doubeling
				lemmas)
  
  (th~require-completely 'base)
  
  (cond ((pdsn~open-node-p conclusion)
	 
	 ;; happens if otter is directly called on the open node of a pds
	 (let* ((supports (pds~node-supports conclusion))
		(justification (node~justification conclusion))
		(sponsors (pdsj~sponsors justification))
		(unsponsors (pdsj~unsponsors justification)))
	   
	   ;; justify the conclusion by otter
	   (infer~compute-outline (infer~find-method 'otter) (cons conclusion supports) (list nil))

	   ;; We need to set the sponsors and unsponsors of the new justification explicitly
	   (setf (pdsj~sponsors (node~justification conclusion)) sponsors)
	   (setf (pdsj~unsponsors (node~justification conclusion)) unsponsors)
	   ))
	
	((not (eq (infer~find-method 'otter) (just~method (node~justification conclusion))))
	 (error "The node ~A should be a open node or should have a justification by method otter." conclusion)))

  (let* ((res-proof (tstp~parse-resolution-proof conclusion out-style)))
    (atp~insert-resolution-proof res-proof conclusion
				 :sspu-style sspu-style
				 :indirect-proof indirect-proof
				 :maximal-depth maximal-depth
				 :integral-formulas integral-formulas
				 :reach-sspu-style sspu-style
				 :avoid-doubeling avoid-doubeling
				 :atp-sign 'otter
				 :lemmas lemmas)))

