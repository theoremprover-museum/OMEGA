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



(mod~defmod ATP 
            :uses (arg blik comint delta env f2p infer inter just keim mixin node omega otter p2f p2pl pds pdsj pdsn pl2p prob prot r2ntop res res2nd satch socket spass th tps eqp)
            :documentation "Handling the usage of atp's in Omega."
            :exports (
                      
		      atp~bliksem-bbox-expansion-function
                      atp~bliksem-compute-bliksem-just
		      atp~eqp-bbox-expansion-function
                      atp~eqp-compute-eqp-just
		      atp~wald-bbox-expansion-function
                      atp~wald-compute-wald-just
                      atp~call-res-based-prover
                      atp~call-satchmo
                      atp~choose-atp-problem-to-transform
                      atp~compute-concurrent-atp-problems
		      atp~compute-concurrent-fo-atp-problems
                      atp~compute-loui-string
		      atp~compute-loui-string-fo
                      atp~expand-resolution-proof-in-its-upper-object
		      atp~insert-resolution-proof
                      atp~interpret-string-from-loui
                      atp~otter-bbox-expansion-function
                      atp~otter-compute-otter-just
                      atp~pl-atp-bbox-expansion-function
                      atp~pl-atp-compute-just
                      atp~protein-bbox-expansion-function
                      atp~protein-compute-protein-just
                      atp~satchmo-bbox-expansion-function
                      atp~satchmo-compute-satchmo-just
                      atp~spass-bbox-expansion-function
                      atp~spass-compute-spass-just
                      atp~read-and-transform-proof
		      
                      atp*original-proof-plan
                      atptop*interactivity-allowed))

;;; The following functions are internal in other modules and should not be used:
;;; (pds=label-counter)


(defvar atp*original-proof-plan nil) ;; the variable in which the omega*current-proof-plan is saved

(defvar atp*original-sponsors nil)
(defvar atp*original-unsponsors nil) ;; two golbal variables to store sponsor and unsponsor list in


#| --------------------------------------------  Handling JUSTS ------------------------------------------------------------------- |#


#| Um supports und so weiter sinnvoll handhaben zu koennen und speichern zu koennen, muss man die open-justs vor einem ATP Aufruf
aufheben. Dies geschieht einfach dadruch, dass die alte-just in die PLIST der neuen als OLD-JUST eingetragen wird. Schlaegt der
ATP-Aufruf fehl, so wird die alte open just wieder restauriert, die neue wird weggeschmissen, falls der Knoten dadurch offen wird (alte
just war offene just), wird der Knoten wieder in die offenen Knoten der PDS eingetragen und der aus den Supports der PDS gestrichen
(siehe atp~call-res-based-prover + atp~call-satchmo). War der Aufruf ein Erfolg, so wird die alte Just weggeschmissen (siehe
atp~call-res-based-prover + atp~call-satchmo).|#



(defun atp=insert-just! (node new-just)  
  (declare (edited  "05-AUG-1998")
	   (authors Ameier)
	   (input   "A node and a justification.")
	   (effect  "The justification of the node is set on new-just, and new-just contains in its PLIST the old just as the"
		    "entry old-just.")
	   (value   "Undefined."))
  (keim~put node 'old-just (node~justification node))
  (pdsn~replace-just! node new-just)
  
  ;; AM (let* ((old-just (node~justification node)))
  ;; (pdsj~replace-justification! old-just new-just)
  ;; (keim~put new-just 'old-just old-just)
  ;; (setf (node~justification node) new-just)))
  
  )

(defun atp=remove-old-just! (node)
  (declare (edited  "05-AUG-1998")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "The plist entry old-just of the justification is deleted.")
	   (value   "UNdefined."))
  (keim~remprop node 'old-just))

(defun atp=restore-old-just! (node)
  (declare (edited  "05-AUG-1998")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "The current justification is removed from the chain of justifications and the"
		    "justification one below the current gets the new current justification.")
	   (value   "Undefined."))
  (let* ((just (node~justification node))
	 (old-just (keim~get node 'old-just)))
    (if (null old-just)
	(pdsn~replace-just! node (pdsj~open-just-create))
      (pdsn~replace-just! node old-just))
    
    (when (pdsj~open-just-p (node~justification node))
      (setf (pds~open-nodes omega*current-proof-plan)
	    (cons node (pds~open-nodes omega*current-proof-plan))) ;; Knoten eingetragen in die offenen Knoten
      ;; AM (setf (pds~support-nodes omega*current-proof-plan)
      ;;	    (remove node (pds~support-nodes omega*current-proof-plan))) ;; Knoten aus Supports der PDS entfernt
      )
    
    ;; AM (setf (pdsj~sponsors (node~justification node)) atp*original-sponsors)
    ;; AM (setf (pdsj~unsponsors (node~justification node)) atp*original-unsponsors)
    
    ))

#| ------------------------------------------- Handling resolution based provers ------------------------------------------------- |#

(defun atp=untested-test (node)
  (if (string-equal (pdsj~status (node~justification node)) "untested") 
      't
    nil))

(defun atp~call-res-based-prover (in-node dir
					   expand
					   ressource
					   atp-infs 
					   &key
					   (sspu-style 'aut)
					   (indirect-proof nil)
					   (maximal-depth 2)
					   (integral-formulas nil)
					   (reach-sspu-style 'case)
					   (avoid-doubeling nil)
					   (lemmas 'constants))
  
  ;; Simplification of true and false, the justification is changed, simplified lines replace the original lines
  ;; The simplification is done only if the statuts of the justification of the in-node is untested!

  (multiple-value-bind
      (already-done new-conc-node)
      (atp=simplifications-check! in-node)
    
    (if already-done
	nil
      (let* ((node (if new-conc-node
		       new-conc-node
		     in-node))
	     (atp-sign (cond ((eq (just~method (node~justification node)) (infer~find-method 'otter))
			      'otter)
			     ((eq (just~method (node~justification node)) (infer~find-method 'spass))
			      'spass)
			     ((eq (just~method (node~justification node)) (infer~find-method 'protein))
			      'protein)
			     ((eq (just~method (node~justification node)) (infer~find-method 'pl-atp))
			      'pl-atp)
			     ((eq (just~method (node~justification node)) (infer~find-method 'bliksem))
			      'bliksem)
			     ((eq (just~method (node~justification node)) (infer~find-method 'eqp))
			      'eqp)
			     ((eq (just~method (node~justification node)) (infer~find-method 'waldmeister))
			      'waldmeister)
			     ))
	     (res-proof (cond ((atp=uncovered-lambda-expressions-p (node~formula node))
			       (omega~message "Warning: The node ~A contains a lambda expression that is not covered within a universally or existentially quantification. Hence, an ATP application to this node is not possible (may a beta-normalization of the node could help)." node)
			       nil)
			      ((eq atp-sign 'otter)
			       (if (not (equal dir 'use-otter-out))
				   (let* ((mode (first atp-infs))
					  (proof-object (second atp-infs))
					  (user-flag-string (third atp-infs))
					  (user-weight-string (fourth atp-infs)))
				     (otter~call-otter node
						       omega*current-proof-plan
						       dir
						       ressource
						       mode
						       (if (or (eq expand 'expand) (eq expand 'parse))
							   't
							 nil)
						       proof-object
						       user-flag-string
						       user-weight-string))
				 (otter~use-otter-out node
						      omega*current-proof-plan
						      (first atp-infs)   ;; otter-out-file
						      (second atp-infs)  ;; style
						      )))
			      
			      ((eq atp-sign 'spass)
			       (let* ((auto-mode (first atp-infs))
				      (splitting-level (second atp-infs))
				      (parse-back (third atp-infs)))
				 
				 (spass~call-spass node
						   omega*current-proof-plan
						   dir
						   ressource
						   auto-mode
						   splitting-level
						   (if (or (eq expand 'expand) (eq expand 'parse))
						       't
						     nil)
						   )))
			      ((eq atp-sign 'protein)
			       (prot~call-protein node
						  omega*current-proof-plan
						  dir
						  ressource
						  (if (or (eq expand 'expand) (eq expand 'parse))
						      't
						    nil)))
			      ((eq atp-sign 'bliksem)
			       (blik~call-bliksem node
						  omega*current-proof-plan
						  dir
						  ressource
						  (first atp-infs)
						  (if (or (eq expand 'expand) (eq expand 'parse))
						      't
						    nil)))
			      ((eq atp-sign 'eqp)
			       (eqp~call-eqp node
					     omega*current-proof-plan
					     dir
					     ressource
					     (first atp-infs)
					     (if (or (eq expand 'expand) (eq expand 'parse))
						 't
					       nil)))
			      ((eq atp-sign 'waldmeister)
			       (wald~call-wald node
					       omega*current-proof-plan
					       dir
					       ressource
					       (first atp-infs)
					       (if (or (eq expand 'expand) (eq expand 'parse))
						   't
						 nil)))
			      ((eq atp-sign 'pl-atp)
			       (let* ((mode (first atp-infs))
				      (proof-object (second atp-infs))
				      (user-flag-string (third atp-infs))
				      (user-weight-string (fourth atp-infs)))
				 (otter~call-otter node
						   omega*current-proof-plan
						   dir
						   ressource
						   mode
						   (if (or (eq expand 'expand) (eq expand 'parse))
						       't
						     nil)
						   proof-object
						   user-flag-string
						   user-weight-string
						   :p2pl 't
						   )))
			      
			      )))
	
	(cond ((null res-proof)
	       
	       ;; -> restore old-just -> old-just wieder curent 
	       ;;    -> falls open: -> wieder in den open nodes der PDS
	       ;;                   -> wieder aus den Support-nodes gestrichen
	       
	       ;;(atp=restore-old-just! in-node)
	       (atp=remove-old-just! in-node) ;; -> alte just nicht mehr benoetigt
	       (if (eq node in-node)
		   (pds~open-node! node)
		 (pds~delete-node! node))
	       
	       (omega~message "~% ~A has failed to prove the problem!" atp-sign))
	      
	      ((eq expand 'test)
	       
	       ;; atp is used only for testing, no parse back of the proof and no expansion of the found resolution proof
	       ;; -> status of atp justification keeps untested ! Was ein bisserl bloed is !!
	       ;; Aber wenn ich hier so was wie unparsed hinschreibe, funktioniert das mit dem expandieren nicht mehr !! AMEIER

	       (atp=remove-old-just! in-node) ;; -> alte just nicht mehr benoetigt
	       (setf (pdsj~status (node~justification node)) "untested"))
	      
	      (t
	       ;;    check the used nodes, all supports that aren't used in the resolution proof are removed from
	       ;;    the premises of the atp-justification of node, and in all the not used nodes the atp-reason is
	       ;;    removed

	       (atp=remove-old-just! in-node) ;; -> alte just nicht mehr benoetigt
	       
	       (let* ((really-used-nodes (remove node (atp=get-really-used-nodes res-proof)))
		      (atp-just (node~justification node))
		      (premises-nodes (just~premises atp-just))
		      (not-used-premises (atptop~remove-list really-used-nodes premises-nodes))
		      (atp-reason (pdsj~own-reason (node~justification node))))
		 (setf (just~premises atp-just) really-used-nodes)
		 (mapcar #'(lambda (node)
			     (pdsj~remove-reason! (node~justification node) atp-reason))
			 not-used-premises)
		 (cond ((eq expand 'expand) ;; expansion of the found resolution proof
			(atp~insert-resolution-proof res-proof node
						     :sspu-style sspu-style
						     :indirect-proof indirect-proof
						     :maximal-depth maximal-depth
						     :integral-formulas integral-formulas
						     :reach-sspu-style reach-sspu-style
						     :avoid-doubeling avoid-doubeling
						     :atp-sign atp-sign
						     :lemmas lemmas))
		       (t
			(setf (pdsj~status (node~justification node)) "unexpanded"))))))))))


#| ----------------------------------------------------- SIMPLIFICATIONS --------------------------------------------------------- |#

(defun atp=simplify-forall-sorts-exists-sorts (atp-node)
  (declare (edited  "10-FEB-2002")
	   (authors Ameier)
	   (input   "A node with an atp justifications.")
	   (effect  "Checks all premises of the nodes justification and the node itself, whether"
		    "they contain occurrences of forall-sort and exists-sort. If this is the case"
		    "occurrences of these defined symbols are removed by their definiens.")
	   (value   "If a new atp-node is created (this is the case when the input node contains"
		    "some occurences of forall-sort and exists-sort) this node, otherwise the input node."))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (forall-sort-obj-pre (env~lookup-object 'forall-sort env))
	 (exists-sort-obj-pre (env~lookup-object 'exists-sort env)))
    (if (null forall-sort-obj-pre)
	;; we are in a theory where forall-sort does not yet exists -> do nothing
	atp-node
      (let* ((forall-sort-obj (data~schema-range forall-sort-obj-pre))
	     (exists-sort-obj (data~schema-range exists-sort-obj-pre))
	     (atp-just (node~justification atp-node))
	     (premises (just~premises atp-just))
	     (all-formulas (cons (node~formula atp-node)
				 (mapcar #'node~formula premises))))
	
	(if (data~positions all-formulas #'(lambda (sub)
					     (or (keim~equal sub forall-sort-obj)
						 (keim~equal sub exists-sort-obj))))
	    ;; there are occurences of forall-sort and/or exists-sort
	    (progn
	      ;; node gets opened again!
	      
	      (tacl~init (cons atp-node (just~premises (node~justification atp-node)))) 
	      
	      (let* ((simplified-premises (mapcar #'atp=simplify-forall-sort-exists-sort-in-node premises))
		     (simplified-atp-node (atp=simplify-forall-sort-exists-sort-in-node atp-node :kind 'conclusion))
		     (new-atp-just (pdsj~closed-just-create (just~method atp-just)
							    simplified-premises
							    (list nil))))
		
		(pdsn~replace-just! simplified-atp-node new-atp-just)
		
		(tacl~end)
		
		simplified-atp-node))
	  
	  atp-node)))))


(defun atp=simplify-forall-sort-exists-sort-in-node (node &key (kind 'premise))
  (let* ((env (pds~environment omega*current-proof-plan))
	 (forall-sort-obj (data~schema-range (env~lookup-object 'forall-sort env)))
	 (exists-sort-obj (data~schema-range (env~lookup-object 'exists-sort env)))
	 (forall-sort-def (th~find-assumption 'forall-sort 'post))
	 (exists-sort-def (th~find-assumption 'exists-sort 'post))
	 (forall-sort-definiendum (th~definition-constant forall-sort-def))
	 (forall-sort-definiens (th~ass-node forall-sort-def))
	 (exists-sort-definiendum (th~definition-constant exists-sort-def))
	 (exists-sort-definiens (th~ass-node exists-sort-def))
	 (forall-sort-posses (data~positions (node~formula node) #'(lambda (sub)
								     (data~schema-equal sub forall-sort-definiendum))))
	 (exists-sort-posses (data~positions (node~formula node) #'(lambda (sub)
								     (data~schema-equal sub exists-sort-definiendum))))
	 (new-node1 (if forall-sort-posses
			(if (string-equal kind 'premise)
			    (first (tacl~apply 'defne* (list nil node) (list forall-sort-definiendum forall-sort-definiens forall-sort-posses)))
			  (second (tacl~apply 'defni* (list node nil) (list forall-sort-definiendum forall-sort-definiens forall-sort-posses))))
		      node))
	 (new-node2 (if exists-sort-posses
			(let* ((new-exists-sort-posses (if forall-sort-posses
							   ;; its possible that the expansion of the forall-sort changed the positions
							   ;; -> compute them again!
							   (data~positions (node~formula new-node1)
									   #'(lambda (sub)
									       (data~schema-equal sub exists-sort-definiendum)))
							 exists-sort-posses)))
			  (if (string-equal kind 'premise)
			      (first (tacl~apply 'defne* (list nil new-node1) (list exists-sort-definiendum exists-sort-definiens
										    new-exists-sort-posses)))
			    (second (tacl~apply 'defni* (list new-node1 nil) (list exists-sort-definiendum exists-sort-definiens
										   new-exists-sort-posses)))))
		      new-node1)))
    new-node2))
		    






;; Neuer Simplification FALL: node ist schema -> wir creieren neue Typvarialen und ersetzen die im Knoten !
;; Ausserdem werden premises, die gekappa't sind eliminiert!
;; Moreover, all premises that contain lambda expressions which are not in an existenatial or universal quantification
;; are removed from the supports.

(defun atp=simplifications-check! (in-node)
  (declare (edited  "05-NOV-1998")
	   (authors Ameier)
	   (input   "A node with an atp justifications.")
	   (effect  "Checks all hyps (premises of atp justification) and the node itself,"
		    "whether they contain false and true. If this is the case the according"
		    "nodes are simplified and if hyps replaced in the atp-just by the simplified"
		    "nodes, if the atp-node itself, the simplified node is justififed by the"
		    "atp-just."
		    "If one of the premises simplifies to false itself, the hole proof is closed"
		    "by an indirect proof.")
	   (value   "Multiple-value:"
		    "First: A flag that signs whether the proof is already done or not."
		    "Second: If a new atp-node is created by simplification this node, otherwise nil."))
  (let* ((node (atp=simplify-forall-sorts-exists-sorts in-node))
	 (atp-just (node~justification node))
	 (premises (just~premises atp-just))
	 (false-obj (env~lookup-object 'false (pds~environment omega*current-proof-plan)))
	 (true-obj (env~lookup-object 'true (pds~environment omega*current-proof-plan)))
	 (schematic-premises (remove-if-not #'(lambda (node)
						(term~schema-p (node~formula node)))
					    premises))
	 (non-schematic-premises (remove-if #'(lambda (node)
						(term~schema-p (node~formula node)))
					    premises))
	 (uncovered-lambda-expr-premises (remove-if-not #'(lambda (node)
							    (atp=uncovered-lambda-expressions-p (node~formula node)))
							premises))
	 (covered-lambda-expr-premises (remove-if #'(lambda (node)
						      (atp=uncovered-lambda-expressions-p (node~formula node)))
						  premises))
	 (justification (node~justification node)))
    
    (when schematic-premises
      (omega~message "Warning: Schematic Premises are ignored if an ATP is called! You may should first instantiate the following nodes: ~A" schematic-premises))
    (when uncovered-lambda-expr-premises
      (omega~message "Warning: Premises with lambda-expressions which are not covered in universal or existential qunatifications are ignored if an ATP is called! May beta-normalization could help on the following nodes: ~A" uncovered-lambda-expr-premises))

    ;; node gets opened again!
    (tacl~init (cons node (just~premises (node~justification node))))
    
    (let* ((simplified-non-schematic-premises (mapcar #'(lambda (premise)
							  (if (or (data~substruct-positions false-obj (node~formula premise))
								  (data~substruct-positions true-obj (node~formula premise)))
							      (let* ((out-line (tacl~apply 'simplify (list nil premise) nil)))
								;; AM replaced infer~compute-outline by tacl~apply
								(first out-line))
							    premise))
						      (intersection non-schematic-premises covered-lambda-expr-premises)))
	   (false-premise (find false-obj simplified-non-schematic-premises :test #'(lambda (obj prem)
										      (if (data~equal obj (node~formula prem))
											  't
											nil)))))
      
      (multiple-value-bind
	  (prove-done new-conc-node)
	  (atp=do-simplifications node simplified-non-schematic-premises)
	
	(when (null prove-done)
	  (let* ((new-atp-just (pdsj~closed-just-create (just~method atp-just)
							simplified-non-schematic-premises
							(list nil)))
		 (curr-node (if new-conc-node
				new-conc-node
			      node))
		 )
	    (pdsn~replace-just! curr-node new-atp-just)
	    (omega~message "~%~%THE NODE: ~A ~A" (node~formula curr-node) (node~justification curr-node))))
	
	
	(tacl~end)
	
	(values prove-done
		(if new-conc-node
		    new-conc-node
		  (if (null (eq in-node node))
		      node
		    nil)))))))
			    

(defun atp=uncovered-lambda-expressions-p (formula)
  (declare (edited  "01-FEB-2001")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "Checks whether the formula contains lambda-expressions that are not within an existenatial or"
		    "a universal quantification. If this is the case T is returned, otherwise nil."))
  (let* ((positions-of-lambda-abstractions (data~positions formula #'data~abstr-p)))
    (some #'(lambda (pos)
	      (let* ((last-pos (first (last (pos~number-list pos)))))
		(if (null (= last-pos 1))
		    't
		  (let* ((overterm (data~struct-at-position formula (pos~butlast pos 1))))
		    (if (or (logic~universal-quantification-p overterm)
			    (logic~existential-quantification-p overterm))
			nil
		      't)))))
	  positions-of-lambda-abstractions)))

(defun atp=do-simplifications (node simplified-non-schematic-premises)
  (declare (edited  "16-NOV-1999")
	   (authors Ameier)
	   (input   "A open node, the simplified (non-schematic) premises to prove this goal"
		    "by an ATP.")
	   (effect  "Call tactics, rules to justify the open node by simplifications and so on.")
	   (value   "Multiple-value:"
		    "First: a flag to ign whether the proof is already done"
		    "Second: A new open node if the input open node was changed backwardly (otherwise nil)."))
  
  (let* ((false-obj (env~lookup-object 'false (pds~environment omega*current-proof-plan)))
	 (true-obj (env~lookup-object 'true (pds~environment omega*current-proof-plan)))
	 (false-premise (find false-obj simplified-non-schematic-premises :test #'(lambda (obj prem)
										    (if (data~equal obj (node~formula prem))
											't
										      nil)))))
    (cond (false-premise
	   ;; es gibt bereits einen False knoten -> indirekter Beweis
	   
	   (progn
	     (tacl~apply 'falsee (list node false-premise) nil)
	     (values 't
		     nil)))
	  
	  ((term~schema-p (node~formula node))
	   ;; conclusion node ist schema -> grounde zuerst
	   
	   (let* ((domain (data~schema-domain (node~formula node)))
		  (new-tvs (mapcar #'(lambda (var)
				       (type~generate-type-primitive-with-new-name 'tc 'type+constant
										   (pds~environment omega*current-proof-plan)))
				   domain))
		  (new-atp-node (second (tacl~apply 'kappai (list node nil) (list new-tvs)))))
	     
	     (multiple-value-bind
		 (proof-done-after-goal-simpl-p new-goal-after-further-simpl)
		 (atp=simplify-false-true-in-goal new-atp-node)
	       
	       (values proof-done-after-goal-simpl-p
		       (if new-goal-after-further-simpl
			   new-goal-after-further-simpl
			 new-atp-node))
	       )))
	  
	  (t
	   ;; sonst 
	   
	   (multiple-value-bind
	       (proof-done-after-goal-simpl-p new-goal-after-further-simpl)
	       (atp=simplify-false-true-in-goal node)
	     
	     (values proof-done-after-goal-simpl-p
		     new-goal-after-further-simpl
		     )
	     )))))

(defun atp=open-node! (node &optional (pds omega*current-proof-plan))
  (let ((undone-steps (pds~open-node! node pds)))
    (when (agenda~in-use)
      (let ((pds-nodes (prob~proof-steps pds))
	    (pds-agenda (pds~agenda pds))
	    (new-tasks))
	(dolist (step undone-steps)
	  (let ((step-node (pdsc~an-node step)))
	    (when (find step-node pds-nodes)
	      (setq new-tasks (cons (agenda~create-goal step-node) new-tasks)))))
	(when new-tasks
	  (if (agenda~empty-p pds-agenda)
	      (setf (pds~agenda pds)
		    (agenda~create nil new-tasks nil pds-agenda))
	    (setf (agenda~next-tasks pds-agenda) (append (agenda~next-tasks pds-agenda) new-tasks)
		  (pds~agenda pds) pds-agenda)))))
    ))

(defun atp=simplify-false-true-in-goal (node)
  (declare (edited  "16-NOV-1999")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "The node is may simplified (false/trues are elimintaed) thereby a new node is"
		    "created which inherits the justification of this node.")
	   (value   "Mulitple-value:"
		    "First: A flag signing whether the proof is already done."
		    "Second: If simplifiaction happened the new node."))
  
  (let* ((atp-just (node~justification node))
	 (false-obj (env~lookup-object 'false (pds~environment omega*current-proof-plan)))
	 (true-obj (env~lookup-object 'true (pds~environment omega*current-proof-plan)))
	 (atp-node-has-to-be-simplified (or (data~substruct-positions false-obj (node~formula node))
					    (data~substruct-positions true-obj (node~formula node)))))
    
    (if (null atp-node-has-to-be-simplified)

	(values nil
		nil)
      
      (progn
	(let* ((new-atp-node (second (tacl~apply 'simplify-goal (list node nil) nil))))

	  (if (keim~equal (node~formula new-atp-node) true-obj)
	      ;; ATP-node ist nur noch TRUE => schliesse atp-node direkt mittels trueI
	      (progn
		(tacl~apply 'truei (list new-atp-node) nil)
		(values 't
			new-atp-node))
	    (progn
	      (values nil
		      new-atp-node))))))))


(defun atp~insert-resolution-proof (res-proof node &key
					      (sspu-style 'aut)
					      (indirect-proof nil)
					      (maximal-depth 2)
					      (integral-formulas nil)
					      (reach-sspu-style 'case)
					      (avoid-doubeling nil)
					      (atp-sign 'otter)
					      (lemmas 'constants))
  (declare (edited  "25-NOV-1997")
	   (authors Ameier)
	   (input   "A resolution proof and a node, that is proven by this resolution proof and as keywords"
		    "the informations for transformation and a keyword atp-sign, to sign by what atp the"
		    "resolution proof was found (default otter)."
		    "Remark1: Make sure, that the nodes of the omega*current-proof-plan and the formulas"
		    "         (assumptions/conclusion), that corresponds to each others are named same."
		    "Remark2: If the node is open a justification is created for the node, with method atp-sign"
		    "         and the premises are the used nodes in the resolution proof.")
	   (effect  "The resolution proof is transformed and inserted into the hole omega*current-proof-plan."
		    "If the input node was opened a new atp justification is inserted.")
	   (value   "Undefined."))
  (let* ((really-used-nodes (remove node (atp=get-really-used-nodes res-proof))))
    
    (when (pdsn~open-node-p node)
      ;; justify the conclusion by otter
      (infer~compute-outline (infer~find-method atp-sign) (cons node really-used-nodes) (list nil))
      )
;;      (setf (pds~open-nodes omega*current-proof-plan) (remove node (pds~open-nodes omega*current-proof-plan)))
;;      
;;      ;; the used-nodes and the proven node itself get this otter as reason
;;      (let* ((otter-reason (pds~change-last-plan-step! node)))
;;        (mapcar #'(lambda (nodi)
;;                    (pdsn~insert-reason! nodi otter-reason))
;;                (cons node really-used-nodes)))) 
    
    (setf (pdsj~status (node~justification node)) "expanded")
    (setq atp*original-proof-plan omega*current-proof-plan)
    
    (let* ((atp-just (node~justification node))
	   (orig-env (pds~environment atp*original-proof-plan)))
      
      ;; transform in omega*current-proof-plan
      (res2nd~transform res-proof
			:label-counter (atp=compute-label-counter atp*original-proof-plan)
			:sspu-style sspu-style
			:indirect-proof indirect-proof
			:maximal-depth maximal-depth
			:integral-formulas integral-formulas
			:reach-sspu-style reach-sspu-style
			:avoid-doubeling avoid-doubeling
			:original-conc-node node
			:original-env orig-env
			:lemmas lemmas)
      
      ;; the premises of the atp-just are changed between the translation
      (setf (just~premises atp-just) really-used-nodes)
      
      ;; new constants may are constructed between the translation and have to be inserted into the
      ;; environment of the original pds
      (mapcar #'(lambda (new-const)
		  (let* ((key (intern (keim~name new-const) (find-package :omega))))
		    (env~enter key new-const orig-env)))
	      r2ntop*new-constants)
      
      ;; F.O. -> H.O. in omega*current-proof-plan
      (if (equal atp-sign 'pl-atp)
	  (pl2p~translate omega*current-proof-plan)
	(f2p~translate omega*current-proof-plan))
      
      ;; insert omega*current-proof-plan into the original pds atp*original-proof-plan
      (let* ((names-in-resolution-proof (mapcar #'keim~name (remove-if #'(lambda (node)                             ;; ***
									   (keim~get node 'lemma))                  ;; ***
								       (cons (res~proof-conclusion res-proof)       ;; ***
									     (res~proof-assumptions res-proof)))))  ;; ***
	     ;; Ersetzt fuer Lemmatisierung
	     ;; (mapcar #'keim~name (cons (res~proof-conclusion res-proof)
	     ;;                           (res~proof-assumptions res-proof))))
	     (nodes-in-original-proof (mapcar #'(lambda (name)
						  (pds~label2node name atp*original-proof-plan))
					      names-in-resolution-proof))
	     (nodes-in-sub-proof (mapcar #'(lambda (name)
					     (pds~label2node name omega*current-proof-plan))
					 names-in-resolution-proof))
	     (node-matching-list (mapcar #'(lambda (sub-node orig-node)
					     (list sub-node orig-node))
					 nodes-in-sub-proof nodes-in-original-proof)))
	(atptop~insert-sub-proof-in-original-proof! atp*original-proof-plan
						    omega*current-proof-plan
						    node-matching-list)

	(when (and r2ntop*tertium-non-datur
		   (find r2ntop*tertium-non-datur (pdsn~hyps (first (first node-matching-list)))))
	  ;; -> enthaelt conc-node des sub-beweises tertium non datur
	  ;; -> fuege tertium non datur in allen Knoten ein, ausser hyps,axioms und Knoten in denen es bereits steht
	  
	  (mapcar #'(lambda (node)
		      (when (null (or (pdsn~hypothesis-node-p node)
				      (find r2ntop*tertium-non-datur (pdsn~hyps node))))
			(setf (pdsn~hyps node)
			      (cons r2ntop*tertium-non-datur (pdsn~hyps node)))))
		  (prob~proof-steps atp*original-proof-plan)))
	
	;; Setze Label-Counter des original Beweises hoch.
	(setf (keim::pds=label-counter atp*original-proof-plan)
	      (keim::pds=label-counter omega*current-proof-plan))
	
	
	;; Die proof plans werden wieder umgesetzt		      
	(setq keim::pds*current-proof-plan atp*original-proof-plan)
	(setq omega*current-proof-plan atp*original-proof-plan)

	;;	(when (and view*on (view~display omega*current-proof-plan))
	;;	  (view~display-proof omega*current-proof-plan))
	
	))))


(defun atp~expand-resolution-proof-in-its-upper-object (res-proof
							prover
							&key
							(sspu-style 'aut)
							(indirect-proof nil)
							(maximal-depth 2)
							(integral-formulas nil)
							(reach-sspu-style 'case)
							(avoid-doubeling nil)
							(lemmas 'constants))
  
  (when (not (pds~proof-plan-p (res~proof-upper-object res-proof)))
    (omega~error "The upper object of resolution proof ~A has to be a pds." res-proof))
  
  (let* ((pds-of-res-proof (res~proof-upper-object res-proof))
	 (node (pds~label2node (keim~name (res~proof-conclusion res-proof)) pds-of-res-proof))
	 (atp-method (infer~find-method prover))
	 (atp-just (first (remove-if-not #'(lambda (just)
					       (eq (just~method just) atp-method))
					   (pdsn~all-justs node))))
	 (premises-nodes (just~premises atp-just)))
    (when (null atp-just)
      (omega~error "Node ~A needs a justification by an atp method (otter\spass)." node))
    
    (setf (pdsj~status atp-just) "expanded")

    (let* ((orig-env (pds~environment pds-of-res-proof)))
      
      ;; transform in omega*current-proof-plan
      (res2nd~transform res-proof
			:label-counter (atptop~compute-label-counter pds-of-res-proof)
			:sspu-style sspu-style
			:indirect-proof indirect-proof
			:maximal-depth maximal-depth
			:integral-formulas integral-formulas
			:reach-sspu-style reach-sspu-style
			:avoid-doubeling avoid-doubeling
			:original-conc-node node
			:original-env orig-env
			:lemmas lemmas)
      
      ;; the premises of the atp-just are changed between the translation
      (setf (just~premises atp-just) premises-nodes) 

      ;; new constants may are constructed between the translation and have to be inserted into the
      ;; environment of the original pds
      (mapcar #'(lambda (new-const)
		  (let* ((key (intern (keim~name new-const) (find-package :omega))))
		    (env~enter key new-const orig-env)))
	      r2ntop*new-constants)
      
      ;; F.O. -> H.O. in omega*current-proof-plan
      (if (equal prover 'pl-atp)
	  (pl2p~translate omega*current-proof-plan)
	(f2p~translate omega*current-proof-plan))
	
      ;; insert omega*current-proof-plan into the original pds pds-of-res-proof
      (let* ((names-in-resolution-proof (mapcar #'keim~name (remove-if #'(lambda (node)                            ;; ***
									   (keim~get node 'lemma))                 ;; ***
								       (cons (res~proof-conclusion res-proof)      ;; ***
									     (res~proof-assumptions res-proof))))) ;; ***
	     ;; angepasst im Zuge der Lemmatisierungseinfuehrung
	     ;; (mapcar #'keim~name (cons (res~proof-conclusion res-proof)
	     ;;                                  (res~proof-assumptions res-proof))))
	     (nodes-in-original-proof (mapcar #'(lambda (name)
						  (pds~label2node name pds-of-res-proof))
					      names-in-resolution-proof))
	     (nodes-in-sub-proof (mapcar #'(lambda (name)
					     (pds~label2node name omega*current-proof-plan))
					 names-in-resolution-proof))
	     (node-matching-list (mapcar #'(lambda (sub-node orig-node)
					     (list sub-node orig-node))
					 nodes-in-sub-proof nodes-in-original-proof)))
	(atptop~insert-sub-proof-in-original-proof! pds-of-res-proof
						    omega*current-proof-plan
						    node-matching-list)
	
	;; Setze Label-Counter des original Beweises hoch
	
	(setf (keim::pds=label-counter pds-of-res-proof)
	      (keim::pds=label-counter omega*current-proof-plan))
	
	;; Die proof plans werden wieder umgesetzt		      
	(setq keim::pds*current-proof-plan pds-of-res-proof)
	(setq omega*current-proof-plan pds-of-res-proof)
	))))



(defun atp=get-really-used-nodes (res-proof)
  (declare (edited  "10-FEB-1997")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "The list of all used lines of the omega*current-proof-plan, that accord to some"
		    "initial clauses of the resolution proof."))
  (let* ((empty-clause (res~proof-empty-clause res-proof))
	 (initial-clauses (res~proof-initial-clauses res-proof))
	 (used-initial-clauses (remove-if #'(lambda (clause)
					      (or (typep clause 'r2ntop+reflex-clause)
						  (typep clause 'r2ntop+flip-clause)
						  (typep clause 'r2ntop+paramod-clause)
						  (typep clause 'r2ntop+transitivity-clause)))
					  (intersection initial-clauses (atptop~get-ancestors empty-clause
											      (node~justification empty-clause)
											      t))))
	 (delta-relation (res~proof-delta-relation res-proof))
	 (pairs (delta~relation-pairs delta-relation))
	 (really-used-formulas-name (atp=get-names-of-formulas used-initial-clauses pairs)))
    
    
    (remove-duplicates (mapcar #'(lambda (name)
				   (pds~label2node name omega*current-proof-plan))
			       really-used-formulas-name)
		       :from-end 't)))


(defun atp=get-names-of-formulas (clauses pairs)
  (declare (edited  "09-OCT-1997")
	   (authors Ameier)
	   (input   "A list of clauses and a list of delta-relation pairs.")
	   (effect  "None.")
	   (value   "A list of the names of the formulas that correspond by the delta-relation"
		    "to the clauses."))
  (do* ((rest-clauses clauses (rest rest-clauses))
	(back-names nil))
      ((null rest-clauses)
       back-names)
    (let* ((head-clause (first rest-clauses))
	   (name-of-pair-of-this-clause
	    (keim~name (delta~delta-formula (first (remove-if-not #'(lambda (pair)
								      (eq head-clause
									  (delta~delta-clause pair)))
								  pairs))))))
      (setq back-names (cons (if (stringp name-of-pair-of-this-clause)
				 (intern (string-upcase name-of-pair-of-this-clause)
					 (find-package :omega)) 
			       name-of-pair-of-this-clause)
			     back-names)))))

(defun atp=compute-label-counter (proof-plan)
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


#| ------------------------------------------------ ATP as Black-Box ------------------------------------------------------ |#

;; outline function
(defun atp~atp-compute-atp-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'atp)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)

;; expansion function
(defun atp~atp-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "Otter is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by ATP call~%" (keim~name (first outline)))
    (let* ((atp-out (first parameters)))
      (cond ((res~proof-p atp-out)
	     (setf atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity

	     (setf (res~proof-upper-object atp-out) omega*current-proof-plan)
	     (mixin~reset-input)
	     
	     (atp~expand-resolution-proof-in-its-upper-object atp-out
							      'atp
							      :sspu-style 'aut
							      :indirect-proof nil
							      :integral-formulas nil
							      :maximal-depth 2
							      :reach-sspu-style 'case
							      :avoid-doubeling nil
							      :lemmas 'constants)

	     (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (t
	     (omega~error "CANNOT BE EXPANDED SO FAR!"))))))


#| ------------------------------------------------ Otter as Black-Box ------------------------------------------------------ |#


;; outline function
(defun atp~otter-compute-otter-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'otter)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)


;; expansion function
(defun atp~otter-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "Otter is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by OTTER call~%" (keim~name (first outline)))
    (let* ((default (first parameters)))
      (if default
	  ;; -> use without interactive requestion directly the default values of all otter-arguments
	  
	  (progn
	    (mixin~reset-input)

	    (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity

	    (atp~call-res-based-prover (first outline)
					(atptop~get-default-directory)
					'expand
					10
					(list 'auto
					      't
					      ""
					      "")
					:sspu-style 'aut
					:indirect-proof nil
					:integral-formulas nil
					:maximal-depth 2
					:reach-sspu-style 'case
					:avoid-doubeling nil
					:lemmas 'constants)
	    
	    (setq atptop*interactivity-allowed 't) ;; allows again interactivity

	    (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	
	;; -> interactively request all arguments for otter
	(let* ((node (first outline))
	       (otter-just (node~justification node))
	       (otter-just-status (pdsj~status otter-just))
	       (status-is-unparsed (if (string= otter-just-status "untested")
				       't
				     nil))
	       (directory (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "The directory for OTTER auxiliary files")
			       (arg~find-argtype 'existing-directory)
			       (atptop~get-default-directory))		      
			    nil))
	       (resolution-proof (if (null status-is-unparsed)
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "The resolution-proof to be expanded")
				      (arg~find-argtype 'resolution-proof)
				      omega*current-resolution-proof)
				   nil))
	       (ressource (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "A time resource in seconds (integer).")
			       (arg~find-argtype 'integer)
			       10)
			    nil))
	       (mode (if status-is-unparsed 
			 (inter~prompt-for-input-with-default
			  (comint~interface comint*current-comint)
			  (format nil "Mode for calling OTTER (auto/user/combined)")
			  (arg~find-argtype 'symbol)
			  'auto)
		       nil))
	       (proof-object (if status-is-unparsed
				 (inter~prompt-for-input-with-default
				  (comint~interface comint*current-comint)
				  (format nil "Use build_proof_object (t/nil)")
				  (arg~find-argtype 'boolean)
				  nil)
			       nil))
	       (user-flag-string (if status-is-unparsed
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "A string of user flag-settings or a file-name")
				      (arg~find-argtype 'string)
				      "")
				   nil))
	       (user-weight-string (if status-is-unparsed
				       (inter~prompt-for-input-with-default
					(comint~interface comint*current-comint)
					(format nil "A string of user weight-settings or a file-name")
					(arg~find-argtype 'string)
					"")
				     nil)) 
	       (sspu-style  (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The SSPU-style (direct/compact/auto)")
			     (arg~find-argtype 'symbol)
			     'auto))
	       (indirect-proof (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "Indirect proof (t/nil)")
				(arg~find-argtype 'boolean)
				nil))
	       (integral-formulas (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Integral formulas (t/nil)")
				   (arg~find-argtype 'boolean)
				   nil))
	       (maximal-depth (if integral-formulas
				  (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Maximal depth of searching integral formulas")
				   (arg~find-argtype 'number)
				   nil)
				2))
	       (tnd-prefer (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Prefer tertium non datur case analyses (t/nil)")
			    (arg~find-argtype 'boolean)
			  t))
	       (avoid-doub (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Avoid doubling (t/nil)")
			    (arg~find-argtype 'boolean)
			    nil))
	       (lemmas (inter~prompt-for-input-with-default
			(comint~interface comint*current-comint)
			(format nil "Lemmas over (nil/constants/full).")
			(arg~find-argtype 'symbol)
			'constants)))
	  (when resolution-proof
	    (setf (res~proof-upper-object resolution-proof) omega*current-proof-plan))
	  
	  (mixin~reset-input)
	  
	  (if status-is-unparsed
	      (progn
		(atp~call-res-based-prover node
					    directory
					    'expand
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
					    :integral-formulas integral-formulas
					    :maximal-depth maximal-depth
					    :reach-sspu-style (if tnd-prefer 'case 'smallest)
					    :avoid-doubeling avoid-doub
					    :lemmas lemmas)
		(set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (progn
	      (atp~expand-resolution-proof-in-its-upper-object resolution-proof
							       'otter
							       :sspu-style  (case sspu-style
									      (direct 'dir)
									      (compact 'sspu)
									      (auto 'aut))
							       :indirect-proof indirect-proof
							       :integral-formulas integral-formulas
							       :maximal-depth maximal-depth
							       :reach-sspu-style (if tnd-prefer 'case 'smallest)
							       :avoid-doubeling avoid-doub
							       :lemmas lemmas)
	      (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    ))))))

#| ----------------------------------------------- BLIKSEM as BLACK BOX ----------------------------------------------------- |#

;; outline function
(defun atp~bliksem-compute-bliksem-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'bliksem)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)

;; expansion function
(defun atp~bliksem-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "Bliksem is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by BLIKSEM call~%" (keim~name (first outline)))
    (let* ((default (first parameters)))
      (if default
	  ;; -> use without interactive requestion directly the default values of all bliksem-arguments
	  
	  (progn
	    (mixin~reset-input)

	    (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity

	    (atp~call-res-based-prover (first outline)
					(atptop~get-default-directory)
					'expand
					10
					(list "")
					:sspu-style 'aut
					:indirect-proof nil
					:integral-formulas nil
					:maximal-depth 2
					:reach-sspu-style 'case
					:avoid-doubeling nil
					:lemmas 'constants)
	    
	    (setq atptop*interactivity-allowed 't) ;; allows again interactivity
	    
	    (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	
	;; -> interactively request all arguments for BLIKSEM
	(let* ((node (first outline))
	       (bliksem-just (node~justification node))
	       (bliksem-just-status (pdsj~status bliksem-just))
	       (status-is-unparsed (if (string= bliksem-just-status "untested")
				       't
				     nil))
	       (directory (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "The directory for BLIKSEM auxiliary files")
			       (arg~find-argtype 'existing-directory)
			       (atptop~get-default-directory))		      
			    nil))
	       (resolution-proof (if (null status-is-unparsed)
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "The resolution-proof to be expanded")
				      (arg~find-argtype 'resolution-proof)
				      omega*current-resolution-proof)
				   nil))
	       (ressource (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "A time resource in seconds (integer).")
			       (arg~find-argtype 'integer)
			       10)
			    nil))
	       (command-string (if status-is-unparsed
				   (inter~prompt-for-input-with-default
				    (comint~interface comint*current-comint)
				    (format nil "A commando setting string")
				    (arg~find-argtype 'string)
				    "")
				 nil))
	       (sspu-style  (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The SSPU-style (direct/compact/auto)")
			     (arg~find-argtype 'symbol)
			     'auto))
	       (indirect-proof (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "Indirect proof (t/nil)")
				(arg~find-argtype 'boolean)
				nil))
	       (integral-formulas (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Integral formulas (t/nil)")
				   (arg~find-argtype 'boolean)
				   nil))
	       (maximal-depth (if integral-formulas
				  (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Maximal depth of searching integral formulas")
				   (arg~find-argtype 'number)
				   nil)
				2))
	       (tnd-prefer (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Prefer tertium non datur case analyses (t/nil)")
			    (arg~find-argtype 'boolean)
			  t))
	       (avoid-doub (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Avoid doubling (t/nil)")
			    (arg~find-argtype 'boolean)
			    nil))
	       (lemmas (inter~prompt-for-input-with-default
			(comint~interface comint*current-comint)
			(format nil "Lemmas over (nil/constants/full).")
			(arg~find-argtype 'symbol)
			'constants)))
	  
	  (when resolution-proof
	    (setf (res~proof-upper-object resolution-proof) omega*current-proof-plan))

	  (mixin~reset-input)
	  
	  (if status-is-unparsed
	      (progn
		(atp~call-res-based-prover node
					    directory
					    'expand
					    ressource
					    (list command-string)
					    :sspu-style (case sspu-style
							  (direct 'dir)
							  (compact 'sspu)
							  (auto 'aut))
					    :indirect-proof indirect-proof
					    :integral-formulas integral-formulas
					    :maximal-depth maximal-depth
					    :reach-sspu-style (if tnd-prefer 'case 'smallest)
					    :avoid-doubeling avoid-doub
					    :lemmas lemmas)
		(set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (progn
	      (atp~expand-resolution-proof-in-its-upper-object resolution-proof
							       'bliksem
							       :sspu-style  (case sspu-style
									      (direct 'dir)
									      (compact 'sspu)
									      (auto 'aut))
							       :indirect-proof indirect-proof
							       :integral-formulas integral-formulas
							       :maximal-depth maximal-depth
							       :reach-sspu-style (if tnd-prefer 'case 'smallest)
							       :avoid-doubeling avoid-doub
							       :lemmas lemmas)
	      (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    ))))))


#| ----------------------------------------------- EQP as BLACK BOX ----------------------------------------------------- |#

;; outline function
(defun atp~eqp-compute-eqp-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'eqp)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)

;; expansion function
(defun atp~eqp-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "EQP is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by EQP call~%" (keim~name (first outline)))
    (let* ((default (first parameters)))
      (if default
	  ;; -> use without interactive requestion directly the default values of all EQP-arguments
	  
	  (progn
	    (mixin~reset-input)

	    (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity

	    (atp~call-res-based-prover (first outline)
					(atptop~get-default-directory)
					'expand
					10
					(list "")
					:sspu-style 'aut
					:indirect-proof nil
					:integral-formulas nil
					:maximal-depth 2
					:reach-sspu-style 'case
					:avoid-doubeling nil
					:lemmas 'constants)
	    
	    (setq atptop*interactivity-allowed 't) ;; allows again interactivity
	    
	    (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	
	;; -> interactively request all arguments for EQP
	(let* ((node (first outline))
	       (eqp-just (node~justification node))
	       (eqp-just-status (pdsj~status eqp-just))
	       (status-is-unparsed (if (string= eqp-just-status "untested")
				       't
				     nil))
	       (directory (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "The directory for EQP auxiliary files")
			       (arg~find-argtype 'existing-directory)
			       (atptop~get-default-directory))		      
			    nil))
	       (resolution-proof (if (null status-is-unparsed)
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "The resolution-proof to be expanded")
				      (arg~find-argtype 'resolution-proof)
				      omega*current-resolution-proof)
				   nil))
	       (ressource (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "A time ressource in seconds (integer).")
			       (arg~find-argtype 'integer)
			       10)
			    nil))
	       (command-string (if status-is-unparsed
				   (inter~prompt-for-input-with-default
				    (comint~interface comint*current-comint)
				    (format nil "A commando setting string")
				    (arg~find-argtype 'string)
				    "")
				 nil))
	       (sspu-style  (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The SSPU-style (direct/compact/auto)")
			     (arg~find-argtype 'symbol)
			     'auto))
	       (indirect-proof (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "Indirect proof (t/nil)")
				(arg~find-argtype 'boolean)
				nil))
	       (integral-formulas (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Integral formulas (t/nil)")
				   (arg~find-argtype 'boolean)
				   nil))
	       (maximal-depth (if integral-formulas
				  (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Maximal depth of searching integral formulas")
				   (arg~find-argtype 'number)
				   nil)
				2))
	       (tnd-prefer (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Prefer tertium non datur case analyses (t/nil)")
			    (arg~find-argtype 'boolean)
			  t))
	       (avoid-doub (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Avoid doubling (t/nil)")
			    (arg~find-argtype 'boolean)
			    nil))
	       (lemmas (inter~prompt-for-input-with-default
			(comint~interface comint*current-comint)
			(format nil "Lemmas over (nil/constants/full).")
			(arg~find-argtype 'symbol)
			'constants)))
	  
	  (when resolution-proof
	    (setf (res~proof-upper-object resolution-proof) omega*current-proof-plan))

	  (mixin~reset-input)
	  
	  (if status-is-unparsed
	      (progn
		(atp~call-res-based-prover node
					    directory
					    'expand
					    ressource
					    (list command-string)
					    :sspu-style (case sspu-style
							  (direct 'dir)
							  (compact 'sspu)
							  (auto 'aut))
					    :indirect-proof indirect-proof
					    :integral-formulas integral-formulas
					    :maximal-depth maximal-depth
					    :reach-sspu-style (if tnd-prefer 'case 'smallest)
					    :avoid-doubeling avoid-doub
					    :lemmas lemmas)
		(set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (progn
	      (atp~expand-resolution-proof-in-its-upper-object resolution-proof
							       'eqp
							       :sspu-style  (case sspu-style
									      (direct 'dir)
									      (compact 'sspu)
									      (auto 'aut))
							       :indirect-proof indirect-proof
							       :integral-formulas integral-formulas
							       :maximal-depth maximal-depth
							       :reach-sspu-style (if tnd-prefer 'case 'smallest)
							       :avoid-doubeling avoid-doub
							       :lemmas lemmas)
	      (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    ))))))

#| ----------------------------------------------- WALDMEISTER as BLACK BOX ----------------------------------------------------- |#

;; outline function
(defun atp~wald-compute-wald-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'waldmeister)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)

;; expansion function
(defun atp~wald-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "WALDMEISTER is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by WALDMEISTER call~%" (keim~name (first outline)))
    (let* ((default (first parameters)))
      (if default
	  ;; -> use without interactive requestion directly the default values of all WALDMEISTER-arguments
	  
	  (progn
	    (mixin~reset-input)

	    (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity

	    (atp~call-res-based-prover (first outline)
					(atptop~get-default-directory)
					'expand
					10
					(list "")
					:sspu-style 'aut
					:indirect-proof nil
					:integral-formulas nil
					:maximal-depth 2
					:reach-sspu-style 'case
					:avoid-doubeling nil
					:lemmas 'constants)
	    
	    (setq atptop*interactivity-allowed 't) ;; allows again interactivity
	    
	    (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	
	;; -> interactively request all arguments for WALDMEISTER
	(let* ((node (first outline))
	       (wald-just (node~justification node))
	       (wald-just-status (pdsj~status wald-just))
	       (status-is-unparsed (if (string= wald-just-status "untested")
				       't
				     nil))
	       (directory (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "The directory for WALDMEISTER auxiliary files")
			       (arg~find-argtype 'existing-directory)
			       (atptop~get-default-directory))		      
			    nil))
	       (resolution-proof (if (null status-is-unparsed)
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "The resolution-proof to be expanded")
				      (arg~find-argtype 'resolution-proof)
				      omega*current-resolution-proof)
				   nil))
	       (ressource (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "A time resource in seconds (integer).")
			       (arg~find-argtype 'integer)
			       10)
			    nil))
	       (command-string (if status-is-unparsed
				   (inter~prompt-for-input-with-default
				    (comint~interface comint*current-comint)
				    (format nil "A commando setting string")
				    (arg~find-argtype 'string)
				    "")
				 nil))
	       (sspu-style  (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The SSPU-style (direct/compact/auto)")
			     (arg~find-argtype 'symbol)
			     'auto))
	       (indirect-proof (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "Indirect proof (t/nil)")
				(arg~find-argtype 'boolean)
				nil))
	       (integral-formulas (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Integral formulas (t/nil)")
				   (arg~find-argtype 'boolean)
				   nil))
	       (maximal-depth (if integral-formulas
				  (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Maximal depth of searching integral formulas")
				   (arg~find-argtype 'number)
				   nil)
				2))
	       (tnd-prefer (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Prefer tertium non datur case analyses (t/nil)")
			    (arg~find-argtype 'boolean)
			  t))
	       (avoid-doub (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Avoid doubling (t/nil)")
			    (arg~find-argtype 'boolean)
			    nil))
	       (lemmas (inter~prompt-for-input-with-default
			(comint~interface comint*current-comint)
			(format nil "Lemmas over (nil/constants/full).")
			(arg~find-argtype 'symbol)
			'constants)))
	  
	  (when resolution-proof
	    (setf (res~proof-upper-object resolution-proof) omega*current-proof-plan))

	  (mixin~reset-input)
	  
	  (if status-is-unparsed
	      (progn
		(atp~call-res-based-prover node
					    directory
					    'expand
					    ressource
					    (list command-string)
					    :sspu-style (case sspu-style
							  (direct 'dir)
							  (compact 'sspu)
							  (auto 'aut))
					    :indirect-proof indirect-proof
					    :integral-formulas integral-formulas
					    :maximal-depth maximal-depth
					    :reach-sspu-style (if tnd-prefer 'case 'smallest)
					    :avoid-doubeling avoid-doub
					    :lemmas lemmas)
		(set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (progn
	      (atp~expand-resolution-proof-in-its-upper-object resolution-proof
							       'waldmeister
							       :sspu-style  (case sspu-style
									      (direct 'dir)
									      (compact 'sspu)
									      (auto 'aut))
							       :indirect-proof indirect-proof
							       :integral-formulas integral-formulas
							       :maximal-depth maximal-depth
							       :reach-sspu-style (if tnd-prefer 'case 'smallest)
							       :avoid-doubeling avoid-doub
							       :lemmas lemmas)
	      (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    ))))))



#| ------------------------------------------------ Spass as Black-Box ------------------------------------------------------ |#


;; outline function
(defun atp~spass-compute-spass-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'spass)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)

;; expansion function
(defun atp~spass-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "Spass is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by Spass call~%" (keim~name (first outline)))
    (let ((default (first parameters)))
      (if default
	  ;; -> use without interactive requestion directly the default values of all spass-arguments
	  
	  (progn
	    (mixin~reset-input)

	    (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity

	    (atp~call-res-based-prover (first outline) 
					(atptop~get-default-directory)
					'expand
					10
					(list 't
					      0)
					:sspu-style 'aut
					:indirect-proof nil
					:integral-formulas nil
					:maximal-depth 2
					:reach-sspu-style 'case 
					:avoid-doubeling nil
					:lemmas 'constants)
	    
	    (setq atptop*interactivity-allowed 't) ;; allows again interactivity

	    (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	
	(let* ((node (first outline))
	       (spass-just (node~justification node))
	       (spass-just-status (pdsj~status spass-just))
	       (status-is-unparsed (if (string= spass-just-status "untested")
				       't
				     nil))
	       (directory (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "The directory for SPASS auxiliary files")
			       (arg~find-argtype 'existing-directory)
			       (atptop~get-default-directory))		      
			    nil))
	       (ressource (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "A time resource in seconds (integer).")
			       (arg~find-argtype 'integer)
			       10)
			    nil))
	       (auto-mode (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "Using auto mode for calling Spass (t/nil)")
			       (arg~find-argtype 'boolean)
			       't)
			    nil))
	       (splitting-level (if (and status-is-unparsed (null auto-mode))
				    (inter~prompt-for-input-with-default
				     (comint~interface comint*current-comint)
				     (format nil "Splitting Levels")
				     (arg~find-argtype 'integer)
				     0)
				  0))
	       (resolution-proof (if (null status-is-unparsed)
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "The resolution-proof to be expanded")
				      (arg~find-argtype 'resolution-proof)
				      omega*current-resolution-proof)
				   nil)) 
	       (sspu-style  (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The SSPU-style (direct/compact/auto)")
			     (arg~find-argtype 'symbol)
			     'auto))
	       (indirect-proof (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "Indirect proof (t/nil)")
				(arg~find-argtype 'boolean)
				nil))
	       (integral-formulas (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Integral formulas (t/nil)")
				   (arg~find-argtype 'boolean)
				   nil))
	       (maximal-depth (if integral-formulas
				  (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Maximal depth of searching integral formulas")
				   (arg~find-argtype 'number)
				   nil)
				2))
	       (tnd-prefer (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Prefer tertium non datur case analyses (t/nil)")
			    (arg~find-argtype 'boolean)
			    t))
	       (avoid-doub (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Avoid doubling (t/nil)")
			    (arg~find-argtype 'boolean)
			    nil))
	       (lemmas (inter~prompt-for-input-with-default
			(comint~interface comint*current-comint)
			(format nil "Lemmas over (nil/constants/full).")
			(arg~find-argtype 'symbol)
			'constants)))
	  
	  (when resolution-proof
	    (setf (res~proof-upper-object resolution-proof) omega*current-proof-plan))
	  (mixin~reset-input)
	  (if status-is-unparsed
	      (progn
		(atp~call-res-based-prover node
					  directory
					  'expand
					  10
					  (list auto-mode
						splitting-level)
					  :sspu-style (case sspu-style
							(direct 'dir)
							(compact 'sspu)
							(auto 'aut))
					  :indirect-proof indirect-proof
					  :integral-formulas integral-formulas
					  :maximal-depth maximal-depth
					  :reach-sspu-style (if tnd-prefer 'case 'smallest)
					  :avoid-doubeling avoid-doub
					  :lemmas lemmas)
		(set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (progn
	      (atp~expand-resolution-proof-in-its-upper-object resolution-proof
							       'spass
							       :sspu-style  (case sspu-style
									      (direct 'dir)
									      (compact 'sspu)
									      (auto 'aut))
							       :indirect-proof indirect-proof
							       :integral-formulas integral-formulas
							       :maximal-depth maximal-depth
							       :reach-sspu-style (if tnd-prefer 'case 'smallest)
							       :avoid-doubeling avoid-doub
							       :lemmas lemmas)
	      (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    ))))))

#| ------------------------------------------------- Protein as Black-Box ------------------------------------------------------------- |#

;; outline function
(defun atp~protein-compute-protein-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'protein)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)

;; expansion function
(defun atp~protein-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "Protein is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by Protein call~%" (keim~name (first outline)))
    
    (let* ((default (first parameters)))
      (if default
	  ;; -> use without interactive requestion directly the default values of all protein-arguments
	  
	  (progn
	    (mixin~reset-input)

	    (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity
	    
	    (atp~call-res-based-prover (first outline)
					(atptop~get-default-directory)
					'expand
					10
					nil
					:sspu-style 'aut
					:indirect-proof nil
					:integral-formulas nil
					:maximal-depth 2
					:reach-sspu-style 'case
					:avoid-doubeling nil
					:lemmas 'constants)
	    
	    (setq atptop*interactivity-allowed 't) ;; allows again interactivity
	    
	    (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	
	;; -> interactively request all arguments for protein
	(let* ((node (first outline))
	       (protein-just (node~justification node))
	       (protein-just-status (pdsj~status protein-just))
	       (status-is-unparsed (if (string= protein-just-status "untested")
				       't
				     nil))
	       (directory (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "The directory for PROTEIN auxiliary files")
			       (arg~find-argtype 'existing-directory)
			       (atptop~get-default-directory))		      
			    nil))
	       (ressource (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "A time resource in seconds (integer).")
			       (arg~find-argtype 'integer)
			       10)
			    nil))
	       (resolution-proof (if (null status-is-unparsed)
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "The resolution-proof to be expanded")
				      (arg~find-argtype 'resolution-proof)
				      omega*current-resolution-proof)
				   nil))
	       (sspu-style  (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The SSPU-style (direct/compact/auto)")
			     (arg~find-argtype 'symbol)
			     'auto))
	       (indirect-proof (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "Indirect proof (t/nil)")
				(arg~find-argtype 'boolean)
				nil))
	       (integral-formulas (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Integral formulas (t/nil)")
				   (arg~find-argtype 'boolean)
				   nil))
	       (maximal-depth (if integral-formulas
				  (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Maximal depth of searching integral formulas")
				   (arg~find-argtype 'number)
				   nil)
				2))
	       (tnd-prefer (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Prefer tertium non datur case analyses (t/nil)")
			    (arg~find-argtype 'boolean)
			    t))
	       (avoid-doub (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Avoid doubling (t/nil)")
			    (arg~find-argtype 'boolean)
			    nil))
	       (lemmas (inter~prompt-for-input-with-default
			(comint~interface comint*current-comint)
			(format nil "Lemmas over (nil/constants/full).")
			(arg~find-argtype 'symbol)
			'constants)))
	  
	  (when resolution-proof
	    (setf (res~proof-upper-object resolution-proof) omega*current-proof-plan))
	  (mixin~reset-input)
	  (if status-is-unparsed
	      (progn
		(atp~call-res-based-prover node
					    directory
					    'expand
					    ressource
					    nil
					    :sspu-style (case sspu-style
							  (direct 'dir)
							  (compact 'sspu)
							  (auto 'aut))
					    :indirect-proof indirect-proof
					    :integral-formulas integral-formulas
					    :maximal-depth maximal-depth
					    :reach-sspu-style (if tnd-prefer 'case 'smallest)
					    :avoid-doubeling avoid-doub
					    :lemmas lemmas)
		(set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (progn
	      (atp~expand-resolution-proof-in-its-upper-object resolution-proof
							       'protein
							       :sspu-style  (case sspu-style
									      (direct 'dir)
									      (compact 'sspu)
									      (auto 'aut))
							       :indirect-proof indirect-proof
							       :integral-formulas integral-formulas
							       :maximal-depth maximal-depth
							       :reach-sspu-style (if tnd-prefer 'case 'smallest)
							       :avoid-doubeling avoid-doub
							       :lemmas lemmas)
	      (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    ))))))

#| ------------------------------------------------- Pl-ATP as Black-Box ------------------------------------------------------------- |#

;; outline function
(defun atp~pl-atp-compute-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'pl-atp)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)

(defun atp=default-res-proof-of-type (conclusion type)
  (declare (edited  "14-NOV-2000")
	   (authors Ameier)
	   (input   "A node and an ATP type.")
	   (effect  "None.")
	   (value   "A resolution proof."
		    "If we find in the plist of node a atp-problem of the type with a complete"
		    "resolution proof, this res-proof is returned, othgerwise omega*current-resolution-proof is returned."))
  (let* ((complete-atp-probs (atp=find-complete-problem-p conclusion))
	 (complete-atp-prob-of-type (first (remove-if-not #'(lambda (atp-prob)
							      (string-equal type (atpprb~problem-type atp-prob)))
							  complete-atp-probs))))
    (if complete-atp-prob-of-type
	(atpprb~problem-part-res-proof complete-atp-prob-of-type)
      omega*current-resolution-proof)))
	 
  
;; expansion function
(defun atp~pl-atp-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "Otter is called and the found res-proof is inserted into the omega*current-proof-plan."
		    "Also the omega*current-resolution-proof is set to the new found res-proof."
		    "REMARK: OTTER IS CALLES WITH P2PL Translation instead of P2F !!!"
		    "        Cause here OTTER should be used only as propositional logic prover.")
	   (value   "A list of all new lines, that are inserted newly in omega*current-proof-plan."
		    "(May nil, if the proof ins't expanded.)")) 
  (mixin~save-and-set-input)
  (let* ((before-nodes (prob~proof-steps omega*current-proof-plan)))
    (omega~message "~% Expanding line ~A justified by PL-ATP call~%" (keim~name (first outline)))
    (let* ((default (first parameters)))
      (if default

	  ;; -> use without interactive requestion directly the default values of all otter-arguments
	  
	  (progn
	    (mixin~reset-input)

	    (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity

	    (atp~call-res-based-prover (first outline)
					(atptop~get-default-directory)
					'expand
					10
					(list 'auto
					      't
					      ""
					      "")
					:sspu-style 'aut
					:indirect-proof nil
					:integral-formulas nil
					:maximal-depth 2
					:reach-sspu-style 'case
					:avoid-doubeling nil
					:lemmas 'constants)
	    
	    (setq atptop*interactivity-allowed 't) ;; allows again interactivity

	    (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	
	;; -> interactively request all arguments for otter
	(let* ((node (first outline))
	       (otter-just (node~justification node))
	       (otter-just-status (pdsj~status otter-just))
	       (status-is-unparsed (if (string= otter-just-status "untested")
				       't
				     nil))
	       (directory (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "The directory for PL-ATP/OTTER auxiliary files")
			       (arg~find-argtype 'existing-directory)
			       (atptop~get-default-directory))		      
			    nil))
	       (resolution-proof (if (null status-is-unparsed)
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "The resolution-proof to be expanded")
				      (arg~find-argtype 'resolution-proof)
				      (atp=default-res-proof-of-type (first outline) 'pl-atp))
				   nil))
	       (ressource (if status-is-unparsed
			      (inter~prompt-for-input-with-default
			       (comint~interface comint*current-comint)
			       (format nil "A time resource in seconds (integer).")
			       (arg~find-argtype 'integer)
			       10)
			    nil))
	       (mode (if status-is-unparsed 
			 (inter~prompt-for-input-with-default
			  (comint~interface comint*current-comint)
			  (format nil "Mode for calling PL-ATP/OTTER (auto/user/combined)")
			  (arg~find-argtype 'symbol)
			  'auto)
		       nil))
	       (proof-object (if status-is-unparsed
				 (inter~prompt-for-input-with-default
				  (comint~interface comint*current-comint)
				  (format nil "Use build_proof_object (t/nil)")
				  (arg~find-argtype 'boolean)
				  't)
			       nil))
	       (user-flag-string (if status-is-unparsed
				     (inter~prompt-for-input-with-default
				      (comint~interface comint*current-comint)
				      (format nil "A string of user flag-settings or a file-name")
				      (arg~find-argtype 'string)
				      "")
				   nil))
	       (user-weight-string (if status-is-unparsed
				       (inter~prompt-for-input-with-default
					(comint~interface comint*current-comint)
					(format nil "A string of user weight-settings or a file-name")
					(arg~find-argtype 'string)
					"")
				     nil)) 
	       (sspu-style  (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The SSPU-style (direct/compact/auto)")
			     (arg~find-argtype 'symbol)
			     'auto))
	       (indirect-proof (inter~prompt-for-input-with-default
				(comint~interface comint*current-comint)
				(format nil "Indirect proof (t/nil)")
				(arg~find-argtype 'boolean)
				nil))
	       (integral-formulas (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Integral formulas (t/nil)")
				   (arg~find-argtype 'boolean)
				   nil))
	       (maximal-depth (if integral-formulas
				  (inter~prompt-for-input-with-default
				   (comint~interface comint*current-comint)
				   (format nil "Maximal depth of searching integral formulas")
				   (arg~find-argtype 'number)
				   nil)
				2))
	       (tnd-prefer (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Prefer tertium non datur case analyses (t/nil)")
			    (arg~find-argtype 'boolean)
			  t))
	       (avoid-doub (inter~prompt-for-input-with-default
			    (comint~interface comint*current-comint)
			    (format nil "Avoid doubling (t/nil)")
			    (arg~find-argtype 'boolean)
			    nil))
	       (lemmas (inter~prompt-for-input-with-default
			(comint~interface comint*current-comint)
			(format nil "Lemmas over (nil/constants/full).")
			(arg~find-argtype 'symbol)
			'constants)))
	  (when resolution-proof
	    (setf (res~proof-upper-object resolution-proof) omega*current-proof-plan))
	  (mixin~reset-input)
	  (if status-is-unparsed
	      (progn
		(atp~call-res-based-prover node
					    directory
					    'expand
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
					    :integral-formulas integral-formulas
					    :maximal-depth maximal-depth
					    :reach-sspu-style (if tnd-prefer 'case 'smallest)
					    :avoid-doubeling avoid-doub
					    :lemmas lemmas)
		(set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    (progn
	      (atp~expand-resolution-proof-in-its-upper-object resolution-proof
							       'pl-atp
							       :sspu-style  (case sspu-style
									      (direct 'dir)
									      (compact 'sspu)
									      (auto 'aut))
							       :indirect-proof indirect-proof
							       :integral-formulas integral-formulas
							       :maximal-depth maximal-depth
							       :reach-sspu-style (if tnd-prefer 'case 'smallest)
							       :avoid-doubeling avoid-doub
							       :lemmas lemmas)
	      (set-difference (prob~proof-steps omega*current-proof-plan) before-nodes))
	    ))))))



#| ---------------------------------------------------- SATCHMO AS BLACK BOX ------------------------------------------------------- |#

(defun atp~call-satchmo (node directory ressource)
  (declare (edited  "28-JUL-1998")
	   (authors Ameier)
	   (input   "A node, a directory and a resource.")
	   (effect  "Calls Satchmo, changes mayby the justifications of the node.")
	   (value   "Undefined."))

  ;; AM (setf atp*original-sponsors (pdsj~sponsors (node~justification node)))
  ;; AM (setf atp*original-unsponsors (pdsj~unsponsors (node~justification node)))
  
  (let* ((satchmo-flag (satch~call-satchmo node omega*current-proof-plan directory ressource)))
    
    (cond ((null satchmo-flag)
	   ;; -> restore old-just -> old-just wieder curent 
	   ;;    -> falls open: -> wieder in den open nodes der PDS
	   ;;                   -> wieder aus den Support-nodes gestrichen
	   (atp=restore-old-just! node)
	   )
	  
	  (t
	   
	   (atp=remove-old-just! node) ;; -> alte just nicht mehr benoetigt
	   
	   (setf (pdsj~status (node~justification node)) "unexpanded")))))


	   
;; outline function
(defun atp~satchmo-compute-satchmo-just (outline parameters)
  (let* ((new-just (pdsj~closed-just-create (infer~find-method 'satchmo)
					    (rest outline)
					    parameters
					    "untested"))
	 (conc-node (first outline)))
    (setf (pdsj~control new-just) (pdsj~control (node~justification conc-node)))
    (atp=insert-just! conc-node new-just))
  outline)


;; expansion function
(defun atp~satchmo-bbox-expansion-function (outline parameters)
  (declare (edited  "19-MAR-1998")
	   (authors Ameier)
	   (input   "A list representing the outline and a list representing the parameters.")
	   (effect  "Sacthmo is called.")
	   (value   "Undefined."))
  (mixin~save-and-set-input)
  
  (omega~message "~% Expanding line ~A justified by SATCHMO call~%" (keim~name (first outline)))
  
  (let* ((default (first parameters)))
    (if default
	;; -> use without interactive requestion directly the default values of all SATCHMO-arguments
	
	(progn
	  (mixin~reset-input)

	  (setq atptop*interactivity-allowed nil) ;; forbidds any kind of interactivity
	  
	  (atp~call-satchmo (first outline)
			    (atptop~get-default-directory)
			    10)
	  
	  (setq atptop*interactivity-allowed 't) ;; allows again interactivity
	  )
      
      
      ;; -> interactively request all arguments for SATCHMO
      (let* ((node (first outline))
	     (satchmo-just (node~justification node))
	     (satchmo-just-status (pdsj~status satchmo-just))
	     (directory (if status-is-unparsed
			    (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "The directory for SATCHMO auxiliary files")
			     (arg~find-argtype 'existing-directory)
			     (atptop~get-default-directory))		      
			  nil))
	     (ressource (if status-is-unparsed
			    (inter~prompt-for-input-with-default
			     (comint~interface comint*current-comint)
			     (format nil "A time resource in seconds (integer).")
			     (arg~find-argtype 'integer)
			     10)
			  nil)))
	(mixin~reset-input)
	(when (eq satchmo-just-status "untested")
	  (atp~call-satchmo (first outline)
			    directory
			    ressource))))))



#|--------------------------------------------------------------------------------------------------------------------------------|#
#| -------------------------------------------- THINGS FOR CONCURRENT ATP's ----------------------------------------------------- |#
#|--------------------------------------------------------------------------------------------------------------------------------|#

#| --------------------------------------------------- Computing Cuncurrent problems --------------------------------------------- |#


(defun atp~compute-concurrent-atp-problems (node-to-proof tps-tactic tps-mode pds)
  (declare (edited  "22-APR-1998")
	   (authors Ameier)
	   (input   "A node to proof by concurrent atp's, a tactic and a mode for tps and the current pds.")
	   (effect  "Some atp problems are created and then in the plist of the node will"
		    "be added an entry under 'atp-problems, that contains this list of atp-problems.")
	   (value   "The list of atp-problems."))
  (let* ((assumption-nodes (pds~node-supports node-to-proof)))

    (keim~put node-to-proof 'atp-problems nil)
    
    ;; generiere OTTER-problem und fuege es in plist unter Eintrag 'atp-problems hinzu 
    (otter~generate-otter-problem-default! node-to-proof assumption-nodes pds) 

    ;; generiere PROTEIN-problem und fuege es in plist unter Eintrag 'atp-problems hinzu 
    (prot~generate-protein-problem-default! node-to-proof assumption-nodes pds) 

    ;; generiere SPASS-problem und fuege es in plist unter Eintrag 'atp-problems hinzu 
    (spass~generate-spass-problem-default! node-to-proof assumption-nodes pds)


    ;; generiere BLIKSEM-PROBLEM und fuege es in plist unter Eintrag 'atp-problems hinzu
    (blik~generate-bliksem-problem-default! node-to-proof assumption-nodes pds)

    ;; generiere EQP-PROBLEM und fuege es in plist unter Eintrag 'atp-problems hinzu
    (eqp~generate-eqp-problem-default! node-to-proof assumption-nodes pds)

    ;; generiere WALDMEISTER-PROBLEM und fuege es in plist unter Eintrag 'atp-problems hinzu
    (wald~generate-wald-problem-default! node-to-proof assumption-nodes pds)

    (tps~generate-tps-problem-default! node-to-proof tps-tactic tps-mode pds)
    
    ;; hier muss noch LEO rein
    
    
    ))

(defun atp~compute-concurrent-fo-atp-problems (node-to-proof tps-tactic tps-mode pds)
  (declare (edited  "22-APR-1998")
	   (authors Ameier)
	   (input   "A node to proof by concurrent atp's, a tactic and a mode for tps and the current pds.")
	   (effect  "Some atp problems are created and then in the plist of the node will"
		    "be added an entry under 'atp-problems, that contains this list of atp-problems.")
	   (value   "The list of atp-problems."))
  (let* ((assumption-nodes (pds~node-supports node-to-proof)))

    (keim~put node-to-proof 'atp-problems nil)
    
    ;; generiere OTTER-problem und fuege es in plist unter Eintrag 'atp-problems hinzu 
    (otter~generate-otter-problem-default! node-to-proof assumption-nodes pds) 

    ;; generiere PROTEIN-problem und fuege es in plist unter Eintrag 'atp-problems hinzu 
    (prot~generate-protein-problem-default! node-to-proof assumption-nodes pds) 

    ;; generiere SPASS-problem und fuege es in plist unter Eintrag 'atp-problems hinzu 
    (spass~generate-spass-problem-default! node-to-proof assumption-nodes pds)


    ;; generiere BLIKSEM-PROBLEM und fuege es in plist unter Eintrag 'atp-problems hinzu
    (blik~generate-bliksem-problem-default! node-to-proof assumption-nodes pds)

    ;; generiere EQP-PROBLEM und fuege es in plist unter Eintrag 'atp-problems hinzu
    (eqp~generate-eqp-problem-default! node-to-proof assumption-nodes pds)

    ;; generiere WALDMEISTER-PROBLEM und fuege es in plist unter Eintrag 'atp-problems hinzu
    (wald~generate-wald-problem-default! node-to-proof assumption-nodes pds)

    ))


#| Der LOUI-STRING sieht folgendermassen aus:

   "atp(otter:\"otter-string\" protein:\"protein-string\" ...)"
|#

(defun atp~compute-loui-string (node-to-proof tps-tactic tps-mode pds)
  (declare (edited  "22-APR-1998")
	   (authors Ameier)
	   (input   "A node to proof by concurrent atp's a tactic for TPS and a mode for
TPS and the current pds.")
	   (effect  "Some atp problems are created and then in the plist of the node will"
		    "be added an entry under 'atp-problems, that contains this list of atp-problems.")
	   (value   "The in-strings of the atps are uned into one LOUI-STRING, this string is reurned."))

  ;; compute the atp-problems
  (atp~compute-concurrent-atp-problems node-to-proof tps-tactic tps-mode pds)

  (let ((loui-string "atp("))

    (do* ((rest-problems (keim~get node-to-proof 'atp-problems) (rest rest-problems)))
	((null rest-problems) loui-string)
      (let* ((atp-problem (first rest-problems))
	     (in-string (atpprb~problem-atp-in-string atp-problem))
	     (type (atpprb~problem-type atp-problem)))
	(cond ((equal type 'otter)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aotter:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aotter:\"~A\" " loui-string in-string))))
	      ((equal type 'protein)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aprotein:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aprotein:\"~A\" " loui-string in-string))))
	      ((equal type 'spass)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aspass:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aspass:\"~A\" " loui-string in-string))))
	      ((equal type 'bliksem)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Abliksem:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Abliksem:\"~A\" " loui-string in-string))))
	      ((equal type 'eqp)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aeqp:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aeqp:\"~A\" " loui-string in-string))))
	      ((equal type 'waldmeister)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Awaldmeister:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Awaldmeister:\"~A\" " loui-string in-string))))
	      ((equal type 'tps)
	       (let ((tps-string (write-to-string (format nil "~A*TPS-SPLITTER*~A" in-string (write-to-string (atpprb~tps-problem-file atp-problem))))))
		 (if (= (length rest-problems) 1)
		     (setq loui-string (format nil "~Atps:~A)"
					       loui-string
					       tps-string))
		   (setq loui-string (format nil "~Atps:~A"
					     loui-string
					     tps-string))))))))))

(defun atp~compute-loui-string-fo (node-to-proof tps-tactic tps-mode pds)
  (declare (edited  "22-APR-1998")
	   (authors Ameier)
	   (input   "A node to proof by concurrent atp's a tactic for TPS and a mode for
TPS and the current pds.")
	   (effect  "Some atp problems are created and then in the plist of the node will"
		    "be added an entry under 'atp-problems, that contains this list of atp-problems.")
	   (value   "The in-strings of the atps are uned into one LOUI-STRING, this string is reurned."))

  ;; compute the atp-problems
  (atp~compute-concurrent-fo-atp-problems node-to-proof tps-tactic tps-mode pds)

  (let ((loui-string "atp("))

    (do* ((rest-problems (keim~get node-to-proof 'atp-problems) (rest rest-problems)))
	((null rest-problems) loui-string)
      (let* ((atp-problem (first rest-problems))
	     (in-string (atpprb~problem-atp-in-string atp-problem))
	     (type (atpprb~problem-type atp-problem)))
	(cond ((equal type 'otter)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aotter:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aotter:\"~A\" " loui-string in-string))))
	      ((equal type 'protein)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aprotein:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aprotein:\"~A\" " loui-string in-string))))
	      ((equal type 'spass)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aspass:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aspass:\"~A\" " loui-string in-string))))
	      ((equal type 'bliksem)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Abliksem:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Abliksem:\"~A\" " loui-string in-string))))
	      ((equal type 'eqp)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Aeqp:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Aeqp:\"~A\" " loui-string in-string))))
	      ((equal type 'waldmeister)
	       (if (= (length rest-problems) 1)
		   (setq loui-string (format nil "~Awaldmeister:\"~A\")" loui-string in-string))
		 (setq loui-string (format nil "~Awaldmeister:\"~A\" " loui-string in-string))))
	      ((equal type 'tps)
	       (let ((tps-string (write-to-string (format nil "~A*TPS-SPLITTER*~A" in-string (write-to-string (atpprb~tps-problem-file atp-problem))))))
		 (if (= (length rest-problems) 1)
		     (setq loui-string (format nil "~Atps:~A)"
					       loui-string
					       tps-string))
		   (setq loui-string (format nil "~Atps:~A"
					     loui-string
					     tps-string))))))))))


#| ------------------------------------------------- Take out-strings -------------------------------------------------------------- |#

(defun atp=find-complete-problem-p (node)
  (declare (edited  "08-JUN-2000")
	   (authors Ameier)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "A list of all complete ATP problems of the node."))
  (remove-if-not #'atpprb~complete-p (keim~get node 'atp-problems)))


(defun atp~interpret-string-from-loui (node type)
  (declare (edited  "22-APR-1998")
	   (authors Ameier)
	   (input   "A node and the type of incomming string.")
	   (effect  "Read from socket the loui-string, add it to the atp-problem"
		    "of type TYPE of node NODE. The given string is then parsed." )
	   (value   "Undefined."))
  (omega~message "~%Getting ATP-OUTPUT from LOUI ...~%")
  (let* ((rest-string (socket~read)))
    (cond ((equal type 'otter)
	   (let* ((otter-problem (atpprb~find-problem node 'otter)))
	     (setf (atpprb~problem-atp-out-string otter-problem) rest-string)
	     (when (or atptop*parse-all
		       (null (atp=find-complete-problem-p node)))
	       (otter~complete-otter-problem! otter-problem))))
	  ((equal type 'protein)
	   (let* ((protein-problem (atpprb~find-problem node 'protein)))
	     (setf (atpprb~problem-atp-out-string protein-problem) rest-string)
	     (when (or atptop*parse-all
		       (null (atp=find-complete-problem-p node)))
	       (prot~complete-protein-problem! protein-problem))))
	  ((equal type 'spass)
	   (let* ((spass-problem (atpprb~find-problem node 'spass)))
	     (setf (atpprb~problem-atp-out-string spass-problem) rest-string)
	     (when (or atptop*parse-all
		       (null (atp=find-complete-problem-p node)))
	       (spass~complete-spass-problem! spass-problem))))
	  ((equal type 'bliksem)
	   (let* ((bliksem-problem (atpprb~find-problem node 'bliksem)))
	     (setf (atpprb~problem-atp-out-string bliksem-problem)
		   (format nil "~%found a proof!~%~%~A" rest-string))
	     (when (or atptop*parse-all
		       (null (atp=find-complete-problem-p node)))
	       (blik~complete-bliksem-problem! bliksem-problem))))
	  ((equal type 'eqp)
	   (let* ((eqp-problem (atpprb~find-problem node 'eqp)))
	     (setf (atpprb~problem-atp-out-string eqp-problem) rest-string)
	     (when (or atptop*parse-all
		       (null (atp=find-complete-problem-p node)))
	       (eqp~complete-eqp-problem! eqp-problem))))
	  ((equal type 'waldmeister)
	   (let* ((wald-problem (atpprb~find-problem node 'waldmeister)))
	     (setf (atpprb~problem-atp-out-string wald-problem) rest-string)
	     (when (or atptop*parse-all
		       (null (atp=find-complete-problem-p node)))
	       (wald~complete-wald-problem! wald-problem))))
	  ((equal type 'tps)
	   (let* ((tps-problem (atpprb~find-problem node 'tps)))
	     (setf (atpprb~problem-atp-out-string tps-problem) rest-string)
	     (atpprb~divide-tps-string! tps-problem)
	     (tps~complete-tps-problem! tps-problem))))))



#| ------------------------------------------------ Choose problem to transform ----------------------------------------------------- |#

(defun atp~choose-atp-problem-to-transform (node type)

  (th~require-completely 'base)
;;;  (ozi~error-handler
  (let* ((atp-problems (keim~get node 'atp-problems))
	 (complete-problems (remove-if-not #'atpprb~complete-p atp-problems))
	 (choosen-atp-problem (find type complete-problems :test #'(lambda (it1 it2)
								     (equal it1 (atpprb~problem-type it2)))))
	 (new-sym (gensym "tps"))
	 )

    (when (null choosen-atp-problem)
      (omega~error "~%Error in function atp~choose-atp-problem-to-transform: There exists no complete ATP-problem on node ~A with type ~A"
		   node type))
    
    (cond ((or (equal type 'protein)
	       (equal type 'otter)
	       (equal type 'spass)
	       (equal type 'bliksem)
	       (equal type 'eqp)
	       (equal type 'waldmeister)
	       )
	   (let* ((trans-settings (atpprb~problem-translation-settings
				   choosen-atp-problem))
		  (res-proof (atpprb~problem-part-res-proof choosen-atp-problem)))
	     
	     (if (not (equal (first trans-settings) 'pl-atp))
		 (progn
		   (setq p2f*domain (second trans-settings))
		   (setq p2f*codomain (third trans-settings)))
	       (progn
		 (setq p2pl*domain (second trans-settings))
		 (setq p2pl*codomain (third trans-settings))))
	     
	     ;; justify the conclusion by according type
	     (infer~compute-outline (infer~find-method type) (cons node (pds~node-supports node)) (list nil))
	     
	     (atp~expand-resolution-proof-in-its-upper-object res-proof
							      type
							      :sspu-style 'aut
							      :indirect-proof nil
							      :maximal-depth 2
							      :integral-formulas nil
							      :reach-sspu-style 'case
							      :avoid-doubeling nil
							      :lemmas 'constants)))
	  ((equal type 'tps)
	   (th~require-completely 'tps)
	   (infer~compute-outline (infer~find-method 'tps)
				  (cons node (pds~node-supports node))
				  (list new-sym))
	   (setf (pds~open-nodes omega*current-proof-plan)
		 (remove node (pds~open-nodes omega*current-proof-plan)))
	   (setf tps*problem-list (acons new-sym choosen-atp-problem tps*problem-list)))
	  )))


#| ---------------------------------------------------- READING THE OUTPUT OF AN ATP --------------------------------------------- |#

(defun atp~read-and-transform-proof (in-node file atp-sign
					     &key
					     (sspu-style 'aut)
					     (indirect-proof nil)
					     (maximal-depth 2)
					     (integral-formulas nil)
					     (reach-sspu-style 'case)
					     (avoid-doubeling nil)
					     (lemmas 'constants))
  
  ;; AM (setf atp*original-sponsors (pdsj~sponsors (node~justification in-node)))
  ;; AM (setf atp*original-unsponsors (pdsj~unsponsors (node~justification in-node)))
  
  ;; Simplification of true and false, the justification is changed, simplified lines replace the original lines
  (multiple-value-bind
      (already-done new-conc-node)
      (atp=simplifications-check! in-node)
    
    (if already-done
	nil
      (let* ((node (if new-conc-node
		       new-conc-node
		     in-node))
	     (res-proof (cond ((eq atp-sign 'otter)
			       (otter~read-otter-output node file))
			      ((eq atp-sign 'spass)
			       (spass~read-spass-output node file))
			      ((eq atp-sign 'bliksem)
			       (blik~read-bliksem-output node file))
			      ((eq atp-sign 'waldmeister)
			       (wald~read-wald-output node file))
			      ((eq atp-sign 'eqp)
			       (eqp~read-eqp-output node file))
			      ((eq atp-sign 'protein)
			       (prot~read-protein-output node file)))))
	
	
	(cond ((null res-proof)
	       
	       ;; -> restore old-just -> old-just wieder curent 
	       ;;    -> falls open: -> wieder in den open nodes der PDS
	       ;;                   -> wieder aus den Support-nodes gestrichen
	       (atp=restore-old-just! node)
	       
	       (omega~message "~% ~A has failed to prove the problem!" atp-sign))
	      
	      (t
	       ;;    check the used nodes, all supports that aren't used in the resolution proof are removed from
	       ;;    the premises of the atp-justification of node, and in all the not used nodes the atp-reason is
	       ;;    removed
	       
	       (atp=remove-old-just! node) ;; -> alte just nicht mehr benoetigt
	       
	       (let* ((really-used-nodes (remove node (atp=get-really-used-nodes res-proof)))
		      (atp-just (node~justification node))
		      (premises-nodes (just~premises atp-just))
		      (not-used-premises (atptop~remove-list really-used-nodes premises-nodes))
		      (atp-reason (pdsj~own-reason (node~justification node))))
		 (setf (just~premises atp-just) really-used-nodes)
		 (mapcar #'(lambda (node)
			     (pdsj~remove-reason! (node~justification node) atp-reason))
			 not-used-premises)

		 (atp~insert-resolution-proof res-proof node
					      :sspu-style sspu-style
					      :indirect-proof indirect-proof
					      :maximal-depth maximal-depth
					      :integral-formulas integral-formulas
					      :reach-sspu-style reach-sspu-style
					      :avoid-doubeling avoid-doubeling
					      :atp-sign atp-sign
					      :lemmas lemmas))))))))
