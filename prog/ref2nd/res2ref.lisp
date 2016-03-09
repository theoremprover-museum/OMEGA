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



(mod~defmod RES2REF 
            :uses (cl delta extdelta filter gsub keim lit node omega pos r2ntop ref refopt res subst term)
            :documentation "Translating a resolution proof into a refutation graph."
            :exports (
                      
                      res2ref~get-refutation-graph-from-proof-instance
                      res2ref~get-refutation-graph-from-resolution-proof
                      
                      res2ref*clause-counter
                      res2ref*debug
                      res2ref*env
                      res2ref*ground-subst
                      res2ref*just-counter
                      res2ref*last-extdelta-relation
		      res2ref*last-ref-graph
                      res2ref*var-const-counter))




#| ---------------------------------------------- Global vars --------------------------------------------------------- |#

(defvar res2ref*clause-counter 0)                   ;; counter
(defvar res2ref*just-counter 0)                     ;; counter
(defvar res2ref*var-const-counter 0)                ;; counter
(defvar res2ref*env nil)                            ;; Current gloabl Environment
(defvar res2ref*ground-subst nil)                   ;; Current ground Substitution


(defvar res2ref*debug nil)                          ;; To set this flag will cause the system to give some interesting information
                                                    ;; during the computation of the refutation graph
                                                    ;; especially res2ref*last-ref-graph is set on the produced refutation graph
                                                    ;; and res2ref*last-extdalta-relation is set on the produced delta-relation
(defvar res2ref*last-ref-graph nil)                 ;; to store the last ref-graph in (zum debuggen)
(defvar res2ref*last-extdelta-relation nil)         ;; to store the last extdelta-relation in (zum debuggen)

(defvar res2ref*temp-variables nil)                 ;; to store temporarily in the environment of the resolution proof added variables,
                                                    ;; that are at the end of the transformation removed from the environment

;; wohl nicht mehr noetig:
;; (defvar res2ref*delta nil)
;; (defvar res2ref*possible-matching-pairs nil)

#| -------------------------------------------------- MAIN ------------------------------------------------------------ |#

(defun res2ref~get-refutation-graph-from-proof-instance (resol-proof)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "None.")
	   (value   "A refutation graph according to this resolution proof"
		    "(calls simply the function res2ref~get-refutation-graph-from-resolution-proof"
		    " with an empty extdelta-relation)."))
  (let* ((extdelta-relation (extdelta~create-relation))
	 (new-ref-graph (res2ref~get-refutation-graph-from-resolution-proof resol-proof extdelta-relation)))
    
    (setq res2ref*last-ref-graph new-ref-graph)

    new-ref-graph))


(defun res2ref~get-refutation-graph-from-resolution-proof (res-proof extdelta-relation)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A resolution proof and an extented delta-relation.")
	   (effect  "A new refutation graph is created from the resolution proof."
		    "The extended delta-relation is updated in the way, that it accords"
		    "to this refutation graph (and not longer to the resolution proof).")
	   (value   "The refutation graph."))
  
  (omega~message "~% Creating Refutation-Graph ...")
  
  (when (filter~res-proof-contains-flip-steps-p res-proof)
    (when res2ref*debug
      (omega~message "~%~%~%res2ref*debug: Starting deletion of unecessary flip-steps.")
      (omega~message "~%----------------------------------------------------------"))
    (filter~remove-flip-steps-from-resolution-proof! res-proof)
    ;; loescht waehrend Filterung entstandene property-list Eintrage
    (mapcar #'(lambda (clause)
		(keim~remprops clause (list 'unflip-clause 'unflip-positions 'already-flipped-clauses 'already-flipped-positions)))
	    (res~proof-clauses res-proof)))
  
  (let ((empty-clause (res~proof-empty-clause res-proof)))

    (setq r2ntop*link-counter 0)
    (setq res2ref*clause-counter 0)
    (setq res2ref*just-counter 0)
    (setq res2ref*var-const-counter 0)
    (setq res2ref*env (res~proof-environment res-proof))
    (setq res2ref*ground-subst (subst~create () ()))
    (setq res2ref*temp-variables nil)

    (setq gsub*initial-clause-list nil)
    (setq gsub*initial-tnd-clauses nil)
    
    ;; next line computes the ground substitution of the hole proof and writes in the list gsub*initial-clause-list
    ;; triples of new (ground-subst) initial-clause ,old initial clauses ,the substitution from second to first
    ;; and a predecessor-path from empty-clause to initial-clause

    ;; Compute from this resol-proof the refutation-graph
    (let* (;; 1. Compute the ground substitution
	   (gsub-empty-clause (progn (when res2ref*debug
				       (omega~message "~%~%~%res2ref*debug: Starting with the computation of the ground-substitions.")
				       (omega~message "~%-----------------------------------------------------------------------"))
				     (gsub~proof2ground-sub-proof empty-clause (subst~create () ())
								  (node~justification empty-clause))))
	   ;; 3. Compute the links and so on... for the refutation graph
	   (refutation-graph (progn (when res2ref*debug
				      (omega~message "~%~%~%res2ref*debug: Starting with the computation of the refutation-graph.")
				      (omega~message "~%---------------------------------------------------------------------"))
				    (res2ref=create-ref-graph res-proof gsub-empty-clause)))
	   (initial-clauses (mapcar #'second gsub*initial-clause-list))
	   (ref-clause-nodes (ref~clause-nodes refutation-graph)))

      (when res2ref*debug
	(omega~message "~%~%~%The refutation graph before optimization:")
	(omega~message "~%-----------------------------------------")
	(ref~print-object refutation-graph 't))                          ;; muss wieder raus HACK BY AMEIER             
      
      ;; 3. reset the clauses ground-referenzes slots to nil, cause this information is not longer needed
      ;;    The information is created during the computation of the ground-substitution (gsub~proof2ground-sub-proof)
      (mapcar #'(lambda (clause)
		  (keim~remprop clause 'ground-referenzes))
	      (res~proof-clauses res-proof))

      ;; 4.b update the delta-relation with the new clauses, created for the refutation graph
      (mapcar #'(lambda (clause-node)
		  (if (not (member clause-node initial-clauses)) ;; -> update extdelta-relation with new renamed-clause
		      (res2ref=update-extdelta-relation! clause-node extdelta-relation)))
	      ref-clause-nodes)

      ;; 5.  -> by renaming it could be happens that a clause is replaced by a new renamed clause -> delete such clauses
      ;; from extdelta-relation, because their renamed copy is already in.
      (res2ref=remove-unnecassary-pairs! ref-clause-nodes extdelta-relation)
      
      ;; 6. optimize the refutation graph
      (when res2ref*debug
	(omega~message "~%~%~%res2ref*debug: Starting optimization of the refutation graph.")
	(omega~message "~%-------------------------------------------------------------"))
      (refopt~optimize-ref-graph! refutation-graph extdelta-relation)
      (refopt~delete-flips! refutation-graph)
      (res2ref=delete-unnecessary-clauses! refutation-graph extdelta-relation)

      (when res2ref*debug
	(omega~message "~%~%~%res2ref*debug: res2ref*last-ref-graph is set to the produced refutation graph.")
	(omega~message "~%-------------------------------------------------------------------------------")
	(omega~message "~%~%~%res2ref*debug: res2ref*last-extdelta-relation is set to the produced extdelta-relation.")
	(omega~message "~%---------------------------------------------------------------------------------------")
	(setq res2ref*last-ref-graph refutation-graph)
	(setq res2ref*last-extdelta-relation extdelta-relation))

      (when res2ref*debug
	(omega~message "~%~%~%The refutation graph after all:")
	(omega~message "~%-----------------------------------------")
	(ref~print-object refutation-graph 't))                          ;; muss wieder raus HACK BY AMEIER    

      ;; 7. remove temporarily added variables
      (mapcar #'(lambda (var)
		  (env~remove (keim~name var) (res~proof-environment res-proof)))
	      res2ref*temp-variables)

      ;; (setq global*graph refutation-graph)
      ;; (error "KHG")
      
      refutation-graph
      )))
  
(defun res2ref=delete-unnecessary-clauses! (ref-graph extdelta-relation)
  (declare (edited  "12-NOV-1997")
	   (authors Ameier)
	   (input   "A refutation graph and the current extdelte-relation.")
	   (effect  "All clauses, that are not used in the links are removed from the clauses slot and"
		    "all delta-pairs that accords to such removed clauses are also removed from the"
		    "delta-relation.")
	   (value   "Undefined."))
  (let* ((used-clauses (remove-duplicates (apply 'append (mapcar #'(lambda (link)
								     (mapcar #'lit~clause (ref~get-literals-from-link link)))
								 (ref~links ref-graph)))))
	 (not-used-clauses (remove-if #'(lambda (clause)
					  (find clause used-clauses))
				      (ref~clause-nodes ref-graph)))
	 (delta-pairs (extdelta~relation-pairs extdelta-relation))) 
    (setf (ref~clause-nodes ref-graph) used-clauses)

    (setf (extdelta~relation-pairs extdelta-relation)
	  (remove-if #'(lambda (pair)
			 (find (extdelta~clause pair) not-used-clauses))
		     delta-pairs))))

  
(defun res2ref=remove-unnecassary-pairs! (clause-nodes extdelta-relation)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A list of clauses and ax extended delta-relation.")
	   (effect  "All pairs of clauses ,who aren't in the clause list, are"
		    "removed from the extemded delta-relation.")
	   (value   "Undefined."))
  (setf (delta~relation-pairs extdelta-relation)
	(remove-if-not #'(lambda (pair)
			   (member (extdelta~clause pair) clause-nodes))
		       (extdelta~relation-pairs extdelta-relation))))

(defun res2ref=update-extdelta-relation! (clause-node extdelta-relation)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A clause and an extended delta-relation.")
	   (effect  "In the clause node's plist item history stands"
		    "a clause of the original resolution proof that stands also"
		    "in the extended delta-relation. Now for all input clauses"
		    "pairs are added ,same as these with history-clause, but"
		    "with the input clause in clause-slot.")
	   (value   "Undefined."))

  (cond ((or (typep clause-node 'r2ntop+paramod-clause)
	     (typep clause-node 'r2ntop+reflex-clause)
	     (typep clause-node 'r2ntop+flip-clause)
	     (typep clause-node 'r2ntop+transitivity-clause))
	 ;; -> Paramod, Reflex, Flip und Transitivity clauses haben keine original Clauses und werden auch nicht
	 ;;    in die Extdelta-Relation eingetragen
	 nil)
	(t
	 (let* ((history-clause (r2ntop~trans-clause-history clause-node)))

	   (if (find history-clause gsub*initial-tnd-clauses)
	       ;; initial clause ist TND-Clause
	       ;; -> Extdelta-Pair muss ganz neu erzeugt werden!
	       (progn

		 (when (null r2ntop*tertium-non-datur)
		   (omega~error "~%Error in function res2ref=update-extdelta-relation! r2ntop*tertium-non-datur should be already set."))
		 
		 (let* ((named-term (termix~create-named-term 'tnd-clause-form
							      (data~copy (node~formula r2ntop*tertium-non-datur)
									 :downto '(data+primitive)))))
		   
		   (extdelta~add-pair! extdelta-relation
				       named-term
				       (pos~list-position '(1 0 1))
				       r2ntop*tertium-non-datur
				       clause-node
				       (pos~list-position '(0)))
		   (extdelta~add-pair! extdelta-relation
				       named-term
				       (pos~list-position '(1 0 2 1))
				       r2ntop*tertium-non-datur
				       clause-node
				       (pos~list-position '(1)))
		   ))
	     ;; Extdelta-=Piar muss von original history-clause auf neue Klause angepasst werden      
	     (let* ((pairs (extdelta~relation-pairs extdelta-relation))
		    (pairs-to-history-clause (remove-if-not #'(lambda (pair)
								(eq (extdelta~clause pair) history-clause))
							    pairs)))
	       (mapcar #'(lambda (x)
			   (extdelta~add-pair! extdelta-relation
					       (extdelta~formula x)
					       (extdelta~formula-position x)
					       (extdelta~pdsnode x)
					       clause-node
					       (extdelta~clause-position x)))
		       pairs-to-history-clause)))))))

	 
#| --------------------------------------------- Creating the clauses and the refutation graph ------------------------------------- |#

(defun res2ref=create-ref-graph (res-proof gsub-empty-clause)
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "The resolution proof and the empty-clause of the according"
		    "ground-substituted res-proof.")
	   (effect  "None.")
	   (value   "A new refutation graph is build and returned."))
  (let* ((ordered-triples (r2ntop~order-modulo-test gsub*initial-clause-list :test #'(lambda (item1 item2)
										       (eq (second item1)
											   (second item2)))))
	 (initial-clauses (res2ref=get-initial-clauses ordered-triples))
	 (refutation-graph (ref~refutation-graph-create
			    (intern (string-upcase (format nil "~A-ref-graph" (keim~name res-proof)))
				    (find-package :omega))
			    initial-clauses nil nil (subst~create () ()))))
    (res2ref=create-links initial-clauses refutation-graph gsub-empty-clause)
    (setf (ref~ground-substitution refutation-graph) res2ref*ground-subst)
    refutation-graph))

(defun res2ref=get-initial-clauses (list-of-triple-lists)
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A list of triple-lists."
		    "Each triple consists of a ground-subst initial clause from the ground-substituted"
		    "res-proof, the acoording (not ground subst) clause from the original res-proof"
		    "and the according ground substitution betwenn both."
		    "Each list of triples consists of triples who have all the same original clause.")
	   (effect  "For each triple list a new clause is inserted into the tefutation graph"
		    "Same time the current global ground substitution is updated.")
	   (value   "All new clauses"))
  
  (let ((head-list-of-triples (first list-of-triple-lists)) ;; all triples have the same original initial clause
	(tail-list-of-triples (rest list-of-triple-lists)))
    (if head-list-of-triples
	(let* ((ordered-triples (r2ntop~order-modulo-test head-list-of-triples
							   :test #'(lambda (item1 item2)
								     (and
								      (keim~equal (third item1)
										  (third item2))
								      (= (length (cl~literals (second item1))) 1)))))   
	       ;; the last step orders the triples from the one original-clause after equal-ground-substitutions
	       ;; at least you need only one ground-subst-clause from each ground-subst-class, but you need all
	       ;; ground-clauses to create the links correct later.
	       ;; Unfortenately This works only correct if the clauses are units (look in ground-subst ->TODO)
	       ;; So we test always, also wether we have unit-clauses
	       (ground-clauses-list (mapcar #'(lambda (triple-list)
						(mapcar #'first triple-list))
					    ordered-triples))
	       )
	  
	  (append (mapcar #'(lambda (triple-list ground-clauses)
			      (let* ((new-clause (res2ref=create-initial-clause (first triple-list) ground-clauses)))
				;; only one triple-needed from one triple list, because all triples from one list
				;; have the same original-clause and the same ground-subst, and all ground-clauses stand in
				;; ground-clauses
				
				(when res2ref*debug
				  (omega~message "~%~%res2ref*debug: The following triples have same origin-clause and same ground-substitution:")
				  (omega~message "~%~A" triple-list)
				  (omega~message "~%The corresponding new clause in the refutation graph is: ~A" new-clause))
				new-clause))							  
			  ordered-triples ground-clauses-list)
		  (res2ref=get-initial-clauses tail-list-of-triples)))
      nil)))

(defun res2ref=create-initial-clause (triple ground-clauses)
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "A triple and a set of all ground-clauses."
		    "A triple consists of a ground-subst initial clause from the ground-substituted"
		    "res-proof, the acoording (not ground subst) clause from the original res-proof"
		    "and the according ground substitution betwenn both."
		    "The ground-clauses are all clauses who a related to the original clause (second"
		    "part of triple) of the triple.")
	   (effect  "A new clause is created and insert into the the refutation-graph,"
		    "The current gloabl groundsubst is upodated by this new inserted clauses.")
	   (value   "A new clause."))
  (let* ((original-initial-clause (second triple))
	 (ground-subst (third triple))
	 (vars (subst~domain ground-subst))
	 (new-vars (mapcar #'(lambda (old-var)
			       (res2ref=new-thing old-var res2ref*env))
			   vars))
	 (renaming (subst~create vars new-vars))
	 (ground-subst-for-renamed-clause (subst~compose-substitution ground-subst
								      (subst~create (subst~codomain renaming)
										    (subst~domain renaming)))))
    ;; produce new renamed-clause and checked ground-subst for this clause
    (multiple-value-bind
	(renamed-clause checked-ground-subst)
	(res2ref=create-clause original-initial-clause renaming ground-subst-for-renamed-clause
			       (first triple) ground-clauses)
      ;; update global-ground-subst with checked-ground-subst
      (res2ref=update-global-ground-subst! checked-ground-subst)
      renamed-clause)))
  
(defun res2ref=create-clause (old-clause renaming-for-old-clause ground-subst-for-renamed-clause ground-clause ground-clauses)
  (declare (edited  "10-SEP-1996")
	   (authors Ameier)
	   (input   "An old clause ,a renaming for this, the ground-subst for the renamed"
		    "old clause, the according ground-clause and all acoording ground-clauses"
		    "for the new to be created clause.")
	   (effect  "None.")
	   (value   "A new clause is created who is renamed from old-clause, history and so on"
		    "are set on this new clause."))
  
  ;; VORSICHT!!!
  ;; Hier lag ein Fehler, der mich einen Tag gekostet hat!
  ;; Spass erzeugt bereits paramod,flip,reflex -clauses, die NICHT GROUND SIND!!
  ;; Im weiteren wird aber davon ausgegangen, dass diese Klauseln bereits komplett grounded im Refutation-Graph vorliegen!
  ;; Sonst funktioniert die Skolem-Ersetzerei usw. nicht mehr, ausserdem gibt es keinerlei ForallE die, etwaige
  ;; Variablemn instantiieren wuerde!
  ;; Daher muessen hier neue Literale aus der GROUND CLAUSE, NICHT DER OLD CLAUSE erzeugt werden!!!
  ;; AMEIER (leicht entnervt nach einem Tag Fehlersuche!)
  
  (let* ((new-literals (cond ((or (typep old-clause 'r2ntop+paramod-clause)
				  (typep old-clause 'r2ntop+flip-clause)
				  (typep old-clause 'r2ntop+reflex-clause))
			      (mapcar #'(lambda (lit)
					  (data~copy lit :downto '(data+primitive)))
				      (cl~literals ground-clause)))
			     (t
			      (mapcar #'(lambda (literal)
					  (lit~literal-create (subst~apply renaming-for-old-clause (lit~atom literal))
							      (lit~positive-p literal)))
				      (cl~literals old-clause)))))
	 (new-name (intern (string-upcase (format nil "~A-~A" (keim~name old-clause) (incf res2ref*clause-counter)))
			   (find-package :omega)))
	 (new-just (res~initial-create (intern (string-upcase (format nil "just-~A" (incf res2ref*just-counter)))
					       (find-package :omega))))
	 (new-clause (cond ((typep old-clause 'r2ntop+paramod-clause)
			    (r2ntop~create-paramod-clause new-literals
							  :justification new-just
							  :name new-name
							  :paramod-position (r2ntop~paramod-clause-paramod-position old-clause)
							  :history old-clause
							  :renaming renaming-for-old-clause
							  :ground-clauses ground-clauses))
			   ((typep old-clause 'r2ntop+flip-clause)
			    (r2ntop~create-flip-clause new-literals
						       :justification new-just
						       :name new-name
						       :history old-clause
						       :renaming renaming-for-old-clause
						       :ground-clauses ground-clauses))
			   ((typep old-clause 'r2ntop+reflex-clause)
			    (r2ntop~create-reflex-clause new-literals
							 :justification new-just
							 :name new-name
							 :history old-clause
							 :renaming renaming-for-old-clause
							 :ground-clauses ground-clauses)) 
			   (t
			    (r2ntop~create-trans-clause new-literals
							:justification new-just
							:name new-name
							:history old-clause
							:renaming renaming-for-old-clause
							:ground-clauses ground-clauses))))
	 (checked-ground-subst (gsub~check-ground-subst new-clause ground-subst-for-renamed-clause)))
    
    ;; update the ground-subst of the new-clause
    (setf (r2ntop~trans-clause-ground-subst new-clause) checked-ground-subst)
    
    (values new-clause checked-ground-subst)))


#| --------------------------------------------------- AUXILIARIES ------------------------------------------------------ |#

(defgeneric res2ref=new-thing (old-thing env)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A variable or a constant and an environment.")
	   (effect  "According to the first input a new varaible or a new"
		    "constant is created and added to the environment.")
	   (value   "The new thing."))
  (:method ((old-thing term+constant) env)
	   (term~generate-term-primitive-with-new-name 'c- (term~type old-thing) 'term+constant env))
  (:method ((old-thing term+variable) env)
	   (let* ((new-var (term~generate-term-primitive-with-new-name 'x- (term~type old-thing) 'term+variable env)))
	     (setq res2ref*temp-variables (cons new-var res2ref*temp-variables))
	     new-var)))



(defun res2ref=update-global-ground-subst! (new-ground-subst)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A ground-subst.")
	   (effect  "The global ground-subst res2ref*ground-subst is updated by adding"
		    "this ground-subst.")
	   (value   "Undefined."))
  (setq res2ref*ground-subst
	(subst~create (append (subst~domain res2ref*ground-subst) (subst~domain new-ground-subst))
		      (append (subst~codomain res2ref*ground-subst) (subst~codomain new-ground-subst)))))

 
(defun res2ref=equal-literals-with-ground-subst (literal1 literal2)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "Two literals.")
	   (effect  "none.")
	   (value   "T if the literals are keim~equal after applying"
		    "res2ref*ground-subst on them, otherwise nil."))
  (let ((ground-atom1 (subst~apply res2ref*ground-subst (lit~atom literal1)))
	(ground-atom2 (subst~apply res2ref*ground-subst (lit~Atom literal2))))
    (if (keim~equal ground-atom1 ground-atom2)
	't
      nil)))

(defun res2ref=get-position-in-parent (position-in-clause del-position-in-parent)
  (declare (edited  "29-MAY-1996")
	   (authors Ameier)
	   (input   "The position of a literal in a clause and the position of"
		    "the literal who is deleted in parent-clause.")
	   (effect  "None.")
	   (value   "Computes the position of the according literal in the"
		    "parent-clause."))
  (if (< (pos~first position-in-clause) (pos~first del-position-in-parent))
      position-in-clause
    (pos~list-position (list (+ (pos~first position-in-clause) 1)))))


#| --------------------------------------------------- CREATE LINKS --------------------------------------------------- |#


(defun res2ref=create-links (ref-clauses refutation-graph gsub-empty-clause)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "The clauses of a refutation graph, the refutation graph itself and"
		    "a empty clause as end point of a whole resolution proof.")
	   (effect  "Beginning at the empty-clause, all steps of the resolution proof"
		    "are computed. From them are made links and this are inserted"
		    "into the refutation graph.")
	   (value   "The refutation graph."))
  (multiple-value-bind
      (initial-clauses steps)
      (r2ntop~clauses-and-steps-from-clause gsub-empty-clause (node~justification gsub-empty-clause))
    (declare (ignore initial-clauses))
    (mapcar #'(lambda (step)
		(res2ref=make-ref-graph-link-from-step! (node~justification step)
							refutation-graph
							ref-clauses))
	    steps))
  refutation-graph)

(defun res2ref=make-ref-graph-link-from-step! (just refutation-graph ref-clauses)
  (declare (edited  "30-MAY-1996")
	   (authors Ameier)
	   (input   "A justification the refutation-graph and the set of"
		    "initial clauses of the refutation graph.")
	   (effect  "The links according to this justification are added"
		    "to the ref-graph.")
	   (value   "Undefined."))
  (cond ((or (typep just 'r2ntop+permutation-literals)
	     (typep just 'res+factoring)
	     (typep just 'r2ntop+permutation-complete)
	     )
	 nil)
	(t
	 (let ((list-of-partner-literal-pairs (res2ref=get-partner-literal-pairs just)))
	   (mapcar #'(lambda (partner-literal-pair)
		       (res2ref=create-new-link! (first partner-literal-pair)
						 (second partner-literal-pair)
						 ref-clauses
						 refutation-graph))
		   list-of-partner-literal-pairs)))))

(defun res2ref=create-new-link! (literal1 literal2 ref-clauses refutation-graph)
  (declare (edited  "30-MAY-1996")
	   (authors Ameier)
	   (input   "Two literals who are resolution partners by resolution."
		    "and the set of initial ref-graph clauses.")
	   (effect  "The new link of the according initial literals to the"
		    "input literals, is added to the graph.")
	   (value   "Undefined."))
  (let* ((init-lits1 (res2ref=get-initial-literals literal1 ref-clauses))
	 (init-lits2 (res2ref=get-initial-literals literal2 ref-clauses))
	 (clauses (mapcar #'lit~clause (append init-lits1 init-lits2)))
	 (positions-of-init-lits (mapcar #'(lambda (literal clause)
					     (r2ntop~position-of-literal literal
									  (cl~literals clause)))
					 (append init-lits1 init-lits2) clauses))
	 (new-link (ref~link-create (intern (string-upcase (format nil "link-~A" (incf r2ntop*link-counter)))
					    (find-package :omega))
				    (mapcar #'(lambda (clause position)
						(list clause position))
					    clauses positions-of-init-lits)
				    (subst~create () ()))))
    (when res2ref*debug
      (omega~message "~%~%res2ref*debug: The following link is added in the refutation-graph:")
      (omega~message "~%~A" new-link))
    (res2ref=insert-link-into-ref-graph! new-link refutation-graph)))

(defun res2ref=get-initial-literals (literal ref-clauses)
  (declare (edited  "26-MAR-1996")
	   (authors Ameier)
	   (input   "A literal, standing somewhere in the tree of a"
		    "resolution-proof, and the initial-clauses of the ref-graph.")
	   (effect  "None.")
	   (value   "All literals in the initial-clauses of the ref-graph who accord"
		    "to the literal. The initial-clauses are selected by using the pred-"
		    "path-informations in their plist."))
  (let* ((clause (lit~clause literal))
	 (original-initial-literals (res2ref=get-according-initial-literals literal
									    clause
									    (node~justification clause)))
	 ;; this are the literals from the original-initial-clauses from the gsub-resolution-proof
	 ;; now we must find the literals in the initial ref-clauses
	 (clauses-to-literals (mapcar #'lit~clause original-initial-literals))
	 (positions (mapcar #'(lambda (literal clause)
				(r2ntop~position-of-literal literal (cl~literals clause)))
			    original-initial-literals clauses-to-literals))
	 (according-ref-clauses (mapcar #'(lambda (clause)
					    (res2ref=get-initial-ref-clause clause ref-clauses))
					clauses-to-literals)))
    (mapcar #'(lambda (clause position)
		(r2ntop~term-at-position (cl~literals clause) position))
	    according-ref-clauses positions)))


(defgeneric res2ref=get-according-initial-literals (literal clause just)
  (declare (edited  "29-MAY-1996")
	   (authors Ameier)
	   (input   "A literal, the clause of the literal and those justification." )
	   (effect  "None.")
	   (value   "A list of all literals of initial clauses who accord to this literal."
		    "These literals are computed by going trough the resolution tree"
		    "above the literal."))
  (:method (literal clause (just res+resolution))
	   (let* ((literal-position (r2ntop~position-of-literal literal (cl~literals clause)))
		  (positions (res~resolution-positions just))
		  (parents (res~resolution-clauses just))
		  (parent1 (first parents))
		  (parent2 (second parents))
		  (literals1 (cl~literals parent1))
		  (literals2 (cl~literals parent2)))

	     ;; (format t "~% LITERAL ~A, in clause ~A, at position ~A" literal clause literal-position)
	     
	     (if (<= (pos~first literal-position) (- (length literals1) 2))  ;; -> in parent1
		 (let ((position-in-parent (res2ref=get-position-in-parent literal-position (first positions))))
		   (res2ref=get-according-initial-literals (r2ntop~term-at-position literals1 position-in-parent)
							   parent1 
							   (node~justification parent1)))
	       (let* ((pos-correct (pos~list-position (list (- (pos~first literal-position) (- (length literals1) 1)))))
		      (position-in-parent (res2ref=get-position-in-parent pos-correct (second positions))))
		 (res2ref=get-according-initial-literals (r2ntop~term-at-position literals2 position-in-parent)
							 parent2
							 (node~justification parent2))))))
  (:method (literal clause (just r2ntop+permutation-literals))
	   (let* ((literal-position (r2ntop~position-of-literal literal (cl~literals clause)))
		  (position-number (pos~first literal-position))
		  (parent (r2ntop~get-permutation-parent just))
		  (positions (r2ntop~get-permutation-positions just))
		  (old-position (first positions))
		  (new-position (second positions))
		  (parent-literals (cl~literals parent)))
	     ;; REMARK: new-position is always <= than old-position

	     ;;(format t "~% LITERAL ~A, in clause ~A, at position ~A" literal clause literal-position)
	     ;;(format t "~% PERMUTATION: NEW-POS: ~A, OLD-POS: ~A" new-position old-position)

	     (cond ((or (< position-number (pos~first new-position)) (> position-number (pos~first old-position)))
		    ;;(format t "~%ONE")
		    (res2ref=get-according-initial-literals (r2ntop~term-at-position parent-literals literal-position)
							    parent
							    (node~justification parent)))
		   ((= position-number (pos~first new-position))
		    ;;(format t "~%TWO")
		    (res2ref=get-according-initial-literals (r2ntop~term-at-position parent-literals old-position)
							    parent
							    (node~justification parent)))
		   (t ;; new-position < literal-position <= old-position -> correct position decreaes by 1
		    ;;(format t "~%THREE")
		    (let ((new-position (pos~list-position (list (- position-number 1)))))  
		      (res2ref=get-according-initial-literals (r2ntop~term-at-position parent-literals new-position)
							      parent
							      (node~justification parent)))))))
  (:method (literal clause (just r2ntop+permutation-complete))
	   (let* ((literal-position (r2ntop~position-of-literal literal (cl~literals clause)))
		  (parent (r2ntop~get-permutation-complete-parent just))
		  (position-pairs (r2ntop~get-permutation-complete-position-pairs just))
		  (parent-literals (cl~literals parent))
		  (literal-position-pair (find literal-position position-pairs :test #'(lambda (pos pos-pair)
											 (keim~equal pos (second pos-pair))))))

	     ;; (format t "~% LITERAL ~A, in clause ~A, at position ~A" literal clause literal-position)

	     (res2ref=get-according-initial-literals (r2ntop~term-at-position parent-literals
									      (first literal-position-pair))
						     parent
						     (node~justification parent))))
  ;; paramodulation nicht mehr noetig
  (:method (literal clause (just res+paramodulation))
	   (let* ((literal-position (r2ntop~position-of-literal literal (cl~literals clause)))
		  (mother (res~paramod-mother just))
		  (father (res~paramod-father just))
		  (father-position (res~paramod-father-position just))
		  (mother-literals (cl~literals mother))
		  (father-literals (cl~literals father)))
	     (if (<= (pos~first literal-position) (- (length mother-literals) 1))  ;; -> in mother
		 (res2ref=get-according-initial-literals (r2ntop~term-at-position mother-literals literal-position)
							 mother
							 (node~justification mother))
	       (let* ((pos-correct (pos~list-position (list (- (pos~first literal-position) (length mother-literals)))))
		      (position-in-parent (res2ref=get-position-in-parent pos-correct father-position)))
		 (res2ref=get-according-initial-literals (r2ntop~term-at-position father-literals position-in-parent)
							 father
							 (node~justification father))))))
  (:method (literal clause (just res+factoring))
	   (let* ((literal-position (r2ntop~position-of-literal literal (cl~literals clause)))
		  (positions (res~factoring-positions just))
		  (pos1 (first positions))
		  (pos2 (second positions))
		  (lesser-pos (if (< (first (pos~number-list pos1))
				     (first (pos~number-list pos2)))
				  pos1
				pos2))
		  (greater-pos  (if (< (first (pos~number-list pos1))
				     (first (pos~number-list pos2)))
				    pos2
				  pos1))
		  (parent (res~factoring-clause just))
		  (parent-just (node~justification parent))
		  (parent-literals (cl~literals parent)))

	     ;; (format t "~% LITERAL ~A, in clause ~A, at position ~A" literal clause literal-position)

	     (if (keim~equal literal-position lesser-pos) ;; -> literal is the result-literal of factoring
		 (append (res2ref=get-according-initial-literals (r2ntop~term-at-position parent-literals lesser-pos)
								 parent
								 parent-just)
			 (res2ref=get-according-initial-literals (r2ntop~term-at-position parent-literals greater-pos)
								 parent
								 parent-just))
	       ;; -> literal is any other literal
	       (res2ref=get-according-initial-literals
		(r2ntop~term-at-position parent-literals (res2ref=get-position-in-parent literal-position greater-pos)) 
		parent
		parent-just))))
  (:method (literal clause (just res+initial))
	   ;; (declare (ignore clause))

	   ;; (format t "~% LITERAL ~A, in clause ~A, at position ~A" literal clause (r2ntop~position-of-literal literal (cl~literals clause)))
	   
	   (list literal))
  (:method (literal clause (just res+ur-resolution))
	   (declare (ignore literal clause))
	   (let ((nucleus (first (res~resolution-clauses just))))
	     (res2ref=get-according-initial-literals
	      (r2ntop~term-at-position (cl~literals nucleus) (first (res~resolution-positions just)))
	      nucleus
	      (node~justification nucleus))))
  (:method (literal clause (just res+hyper-resolution))
	   (let* ((literal-position (r2ntop~position-of-literal literal (cl~literals clause)))
		  (lit-pos-number (pos~first literal-position))
		  (parents (res~resolution-clauses just))
		  (nucleus (first parents))
		  (electrons (rest parents))
		  (nucleus-literals (cl~literals nucleus))
		  (positions (res~resolution-positions just))
		  (nucl-lits-without-resolved (if (typep just 'res+negative-hyper-resolution)
						  (remove-if #'lit~positive-p nucleus-literals)
						(remove-if-not #'lit~positive-p nucleus-literals)))
		  (rest-nucl-length (length nucl-lits-without-resolved)))
	     
	     
	     (if (< lit-pos-number rest-nucl-length)
		 (res2ref=get-according-initial-literals
		  (data~struct-at-position nucl-lits-without-resolved literal-position)
		  nucleus
		  (node~justification nucleus))
	       (multiple-value-bind
		   (literal-parent literal-in-parent)
		   (do* ((current-num (- lit-pos-number rest-nucl-length))
			 (rest-electrons electrons (rest rest-electrons))
			 (rest-elec-pos positions (rest rest-elec-pos))
			 (parent nil)
			 ;; (position nil)
			 )
		       (parent
			(values parent literal))

		     (let* ((head-electron (first rest-electrons))
			    (head-elec-pos (first rest-elec-pos))
			    (updated-num (- current-num (- (length (cl~literals head-electron)) 1))))
		       
		       (if (< updated-num 0)
			   ;; literal is from this clause
			   (progn
			     (setq parent head-electron)
			     (setq literal (data~struct-at-position (remove (data~struct-at-position (cl~literals head-electron)
												     head-elec-pos)
									    (cl~literals head-electron))
								    (pos~list-position (list current-num)))))
			 (setq current-num updated-num))))
		 
		 (res2ref=get-according-initial-literals
		  literal-in-parent
		  literal-parent
		  (node~justification literal-parent)))))))




(defgeneric res2ref=get-partner-literal-pairs (just)
  (declare (edited  "29-MAY-1996")
	   (authors Ameier)
	   (input   "A RES Justification.")
	   (effect  "None.")
	   (value   "Returns a list of pairs of literals who are resolved in this"
		    "justification's step."))
  (:method ((just res+paramodulation))
	   (list (list (r2ntop~term-at-position (cl~literals (res~paramod-mother just))
					 (pos~list-position (list (pos~first (res~paramod-mother-position just)))))
		       (r2ntop~term-at-position (cl~literals (res~paramod-father just)) (res~paramod-father-position just)))))
  (:method ((just res+resolution))
	   (let ((parents (res~resolution-clauses just))
		 (positions (res~resolution-positions just)))
	     (list (list (r2ntop~term-at-position (cl~literals (first parents)) (first positions))
			 (r2ntop~term-at-position (cl~literals (second parents)) (second positions))))))
  (:method ((just res+factoring))
	   nil) ;; not needed
  (:method ((just res+ur-resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (nucleus (first parents))
		  (ur-position (first (res~resolution-positions just)))
		  (ur-literal (r2ntop~term-at-position (cl~literals nucleus) ur-position))
		  (other-nucleus-literals (remove ur-literal (cl~literals nucleus))))
	     (mapcar #'(lambda (nucleus-literal electron)
			 (list nucleus-literal (first (cl~literals electron))))
		     other-nucleus-literals (rest parents))))
  (:method ((just res+hyper-resolution))
	   (let* ((parents (res~resolution-clauses just))
		  (positions (res~resolution-positions just))
		  (nucleus (first parents))
		  (nucleus-literals (cl~literals nucleus))
		  (eliminated-literals-in-nucleus (if (typep just 'res+negative-hyper-resolution)
						      (remove-if-not #'lit~positive-p nucleus-literals)
						    (remove-if #'lit~positive-p nucleus-literals))))
	     (mapcar #'(lambda (nucleus-literal electron position)
			 (list nucleus-literal (r2ntop~term-at-position (cl~literals electron) position)))
		     eliminated-literals-in-nucleus (rest parents) positions))))

	     
(defun res2ref=insert-link-into-ref-graph! (new-link refutation-graph)
  (declare (edited  "18-SEP-1996")
	   (authors Ameier)
	   (input   "A link and a refutatoin graph.")
	   (effect  "The links of the refutation graph are changed."
		    "If there are yet links that contain literals of the input link"
		    "in their shores, this links and the input link are uned to"
		    "a new link. This new link is added to the links of the"
		    "refutation graph while all old links now uned with the new"
		    "are removed. If there are no such links the input link is added"
		    "to the links of the refutation graph.")
	   (value   "Undefined."))
  (let* ((links (ref~links refutation-graph))
	 (new-link-shore-elements (append (ref~positive-shore new-link)
					  (ref~negative-shore new-link)))
	 (links-with-shore-elements-from-new-link
	  (remove-if-not #'(lambda (link)
			     (intersection (append (ref~positive-shore link)
						   (ref~negative-shore link))
					   new-link-shore-elements
					   ))
			 links)))
    (when res2ref*debug
      (omega~message "~%~%res2ref*debug: While inserting link ~A in the refutation graph, it is uned with following links" new-link)
      (omega~message "~%~A" links-with-shore-elements-from-new-link))
    (if links-with-shore-elements-from-new-link
	(let* ((new-pos-shore (remove-duplicates
			       (apply 'append (mapcar #'ref~positive-shore
						      (cons new-link links-with-shore-elements-from-new-link)))))
	       (new-neg-shore (remove-duplicates
			       (apply 'append (mapcar #'ref~negative-shore
						      (cons new-link links-with-shore-elements-from-new-link))))))
	  (setf (ref~positive-shore new-link) new-pos-shore)
	  (setf (ref~negative-shore new-link) new-neg-shore)
	  (setf (ref~links refutation-graph)
		(cons new-link (r2ntop~remove-list links-with-shore-elements-from-new-link
						   links)))
	  (when res2ref*debug
	    (omega~message "~%So at last link ~A replaces all others" new-link)))
      
      (ref~add-link-to-graph refutation-graph new-link))))

;; A literal in the ground-subst-resolution-tree accords to an initial clause in this tree 
;; you has to get the initial-clause in the refutation-graph. This clause you can get by checking
;; wether initial-ref-clause has in his plist the initial-ground-subst-clause.

(defun res2ref=get-initial-ref-clause (gsub-proof-clause ref-clauses)
  (declare (edited  "29-MAY-1996")
	   (authors Ameier)
	   (input   "An original ground-subst initial clause and the ref-graph"
		    "clauses.")
	   (effect  "None.")
	   (value   "The ref-graph clause who accords to the ground-subst initial clause"
		    "(who stands in the history-slot of the ref-graph clauses)."))
  (let ((contained (member gsub-proof-clause ref-clauses
			   :test #'(lambda (ground-clause initial-clause)
				     (member ground-clause (r2ntop~trans-clause-ground-clauses initial-clause))))))
    (if contained
	(first contained)
      (omega~error "Function res2ref=get-initial-ref-clause: Something going wrong."))))

