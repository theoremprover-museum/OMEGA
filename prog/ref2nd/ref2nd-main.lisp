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




(mod~defmod RES2ND 
            :uses (adv alift cl data decomp env extdelta infer integ just keim lit logic
		       lord node omega pds pdsj pdsn plco pos prob r2ntop ref res res2ref sspu tacl term termix)
            :documentation "Translating resolution proofs to ND-proofs."
            :exports (
                      
                      res2nd~transform
                      ))

;;; The following functions are internal in other modules and should not be used:
;;; (pds=label-counter)







#| -------------------------------------------------- Auxiliaries -------------------------------------------------- |#

(defgeneric res2nd=clause2node (clause delta-relation)
  (declare (edited  "28-FEB-1996")
	   (authors Ameier)
	   (input   "A clause and an extended delta-relation.")
	   (effect  "If clause is a refexivity clause, a new node is created, added"
		    "to omega*current-proof-plan, and an according delta-pair is added"
		    "to the delta-relation.")
	   (value   "The nd node in pds*current-proof-plan linked to the clause by the delta-relation."))
  (:method ((clause r2ntop+reflex-clause) (delta-relation extdelta+delta-relation)) 
	   (if (keim~get clause 'used-reflex)
	       (let ((atomic-position (pos~add-front 0 (pos~empty))))
		 (nth-value 2 (extdelta~get-atom delta-relation clause atomic-position)))
	     (progn
	       (keim~put clause 't 'used-reflex)
	       (res2nd=introduce-reflex-nodes-into-delta-relation! clause delta-relation))))
  (:method ((clause cl+clause) (delta-relation extdelta+delta-relation))
	   (extdelta~pdsnode (find clause (delta~relation-pairs delta-relation)
				   :test #'(lambda (cl pair)
					     (eq cl (extdelta~clause pair)))))))


(defgeneric res2nd=formula-is-neg-of-formula (formula1 formula2)
  (declare (edited  "29-FEB-1996")
	   (authors Ameier)
	   (input   "Two formulas.")
	   (effect  "None.")
	   (value   "If one formula is the negation of the other a list is returned, that contains"
		    "the not negated formula as first and the negated formula as second,"
		    "nil otherwise."))
  (:method ((formula1 pdsn+node) (formula2 pdsn+node))
	   (let* ((form1 (node~formula formula1))
		  (form2 (node~formula formula2))
		  (erg (res2nd=formula-is-neg-of-formula form1 form2)))
	     (if erg
		 (if (eq (first erg) form1)
		     (list formula1 formula2)
		   (list formula2 formula1))
	       nil)))
  (:method (formula1 formula2)
	   (let* ((not (env~lookup-object 'not (pds~environment keim::pds*current-proof-plan)))
		  (neg-formula1 (term~appl-create not (list formula1)))
		  (neg-formula2 (term~appl-create not (list formula2))))
	     (cond ((data~equal neg-formula1 formula2)
		    (list formula1 formula2))
		   ((data~equal formula1 neg-formula2)
		    (list formula2 formula1))
		   (t
		    nil)))))

(defun res2nd=get-lines-to-refutation-graph (ref-graph extdelta)
  (declare (edited  "10-FEB-1997")
	   (authors Ameier)
	   (input   "A refutation graph and an according delta-relation.")
	   (effect  "None.")
	   (value   "The list of all lines, that are linked to clauses"
		    "of the refutation graph by the delta-relation."))
  (let* ((pairs (extdelta~relation-pairs extdelta))
	 (clauses (ref~clause-nodes ref-graph)))
    (do* ((rest-clauses clauses (rest rest-clauses))
	  (back-lines nil))
	((null rest-clauses) (remove-duplicates back-lines))
      (let* ((head-clause (first rest-clauses)))
	(when (and (not (typep head-clause 'r2ntop+paramod-clause))
		   (not (typep head-clause 'r2ntop+flip-clause))
		   (not (typep head-clause 'r2ntop+reflex-clause)))
	  (let* ((line-of-clause (extdelta~pdsnode (first (remove-if-not #'(lambda (pair)
									     (eq head-clause (extdelta~clause pair)))
									 pairs)))))
	    (setq back-lines (cons line-of-clause back-lines))))))))


(defun res2nd=check-tertium-non-datur ()
  (declare (edited  "23-MAR-1998")
	   (authors Ameier)
	   (input   "None.")
	   (effect  "It is checked wether tertium-non0datur is really used if it is added to the proof."
		    "If it isn't used finally it is removed from the proof.")
	   (value   "Undefined."))
  (when r2ntop*tertium-non-datur    ;; tertium-non-datur wurde eingefuegt
    (let* ((nodes (prob~proof-steps omega*current-proof-plan))
	   (nodes-using-tertium-non-datur (remove-if-not #'(lambda (node)
							     (find r2ntop*tertium-non-datur (just~premises (node~justification node))))
							 nodes)))
      (when (null nodes-using-tertium-non-datur)
	(setf (prob~proof-steps omega*current-proof-plan) (remove r2ntop*tertium-non-datur
								  (prob~proof-steps omega*current-proof-plan)))
	(mapcar #'(lambda (node)
		    (setf (pdsn~hyps node) (remove r2ntop*tertium-non-datur (pdsn~hyps node))))
		(prob~proof-steps omega*current-proof-plan))))))
  
#| ----------------------------------------------- END HILFSFUNKTIONEN ------------------------------------------------- |#

#| --------------------------------------------- Hauptprogramm --------------------------------------------------------- |#

;; sspu-style should be: aut sspu dir
;; indirect-proof: nil t
;; maximal-depth: a number
;; integral-formulas: nil t
;; reach-sspu-style: case smallest
;; avoid-doubeling: nil t

#|
Alte Version, ersetzt in Folge der Lemmatisierung!
(defun res2nd~transform (res-proof &key
				   (sspu-style 'aut)
				   (indirect-proof nil)
				   (maximal-depth 2)
				   (integral-formulas nil)
				   (reach-sspu-style 'case)
				   (avoid-doubeling nil)
				   (label-counter nil)
				   (original-conc-node nil)
				   (original-env nil))
  (declare (edited  "25-MAR-1996")
	   (authors Ameier)
	   (input   "A resolution proof and by keywords a some settings for the transformation.")
	   (effect  "The resolution-proof is translated into an nd-proof, which is"
		    "inserted into omega*current-proof-plan and keim::pds*current-proof-plan.")
	   (value   "omega*current-proof-plan."))
  
  ;; Setting some global variables
  (setq r2ntop*sspu-style sspu-style)
  (setq integ*maximal-depth maximal-depth)
  (setq r2ntop*integral-formulas integral-formulas)
  (setq r2ntop*avoid-doubeling avoid-doubeling)
  (setq r2ntop*tertium-non-datur nil)
  (setq r2ntop*conclusion-just (if original-conc-node
				   (node~justification original-conc-node)
				 nil))
  (setq r2ntop*new-constants nil)

  (when r2ntop*trans-debug
    (omega~message "~%~%~%r2ntop*trans-debug: Creating the initial PDS.")
    (omega~message "~%---------------------------------------------"))
  
  ;; creates a new PDS with an open line that comes from the conlusion of the resolution proof and
  ;; hypothesis that come from the assumptions of the resolution proof
  ;; omega*current-proof-plan and keim::pds*current-proof-plan are set to this new PDS
  (res2nd=init-pds-from-res-proof res-proof
				  :label-counter label-counter
				  :original-env original-env)
  
  (when r2ntop*trans-debug
    (omega~message "~%~%~%r2ntop*trans-debug: Creating the Extdelta-Relation.")
    (omega~message "~%---------------------------------------------------"))
    (let* ((nodes (prob~proof-steps omega*current-proof-plan))
	 ;; erweitert delta-relation zur extdelta-relation
	 (extdelta-relation (extdelta~add-nodes-to-delta nodes (res~proof-delta-relation res-proof))))
    
    (multiple-value-bind
	(list-of-conclusions list-of-res-proofs list-of-extdeltas)
	;; zerlegt resolution proof durch Lemmatisierung etc. in kleinere Bestandteile -> kann, bzw. muss auch PDS veraendern und
	;; weitere offene Zeilen erzeugen, pro resolution proof eine
	;; Man beachte: An erster Stelle muss immer der Haupt-resolution bzw. Extdelta stehen, d.h. der, der die letztendlich
	;; conclusion schliesst ! -> Braucht man beim Aufruf indirekter Beweise !!
	(res2nd=split-res-proof-and-delta res-proof extdelta-relation)
      
      (when r2ntop*trans-debug
	(omega~message "~%~%~%r2ntop*trans-debug: Creating the Refutation Graphs.")
	(omega~message "~%---------------------------------------------------"))
      
      (let* ((list-of-ref-graphs (mapcar #'(lambda (res-proof extdelta-relation)
					     ;; berechnet refutation graph zu resolution proof und updated die extdelta-relation
					     (res2ref~get-refutation-graph-from-resolution-proof res-proof extdelta-relation))
					 list-of-res-proofs
					 list-of-extdeltas)))
	
	(omega~message "~%~% Translating ...")
	
	;; Some more settings of global variables:
	(if (remove-if-not #'(lambda (clause)
			       (or (typep clause 'r2ntop+paramod-clause)
				   (typep clause 'r2ntop+reflex-clause)
				   (typep clause 'r2ntop+flip-clause)))
			   (apply 'append (mapcar #'(lambda (ref-graph)
						      (ref~clause-nodes ref-graph))
						  list-of-ref-graphs)))    
	    (setq r2ntop*reach-sspu-style 'case)
	  (setq r2ntop*reach-sspu-style reach-sspu-style))
	
	(when r2ntop*trans-debug
	  (omega~message "~%~%~%r2ntop*trans-debug: Creating the initial decompose units.")
	  (omega~message "~%---------------------------------------------------------"))
	
	;; berechnet die Start-decomposition Units
	(let* ((start-units (mapcar #'(lambda (conclusion-node refutation-graph extdelta-relation)
					(decomp~new-decompose-unit conclusion-node
								   refutation-graph
								   extdelta-relation))
				    list-of-conclusions list-of-ref-graphs list-of-extdeltas)))
	  
	  ;; Hier kommen die wirklich vorkommenden Skolem-Functionen rein, wirklich vorkommend heisst, solche, die
	  ;; wirklich nach Filterung, Optimierung usw. noch in Clauseln der Refutation-Graphs vorkommen, und daher auch
	  ;; mittels der entsprechenden extdelta-paare erreichbar sind 
	  ;; -> benutzt in Decompose und Link-ordering !!!
	  (setq r2ntop*skolem-constants
		(remove-duplicates
		 (apply 'append (mapcar #'res2nd=seek-skolem-constants start-units))))
	  ;; ^|koennte man jeweils auch pro refutation graph machen? Noe warum ?! ^|
	  
	  
	  
	  ;; Falls indirect-proof eingestellt -> passe das Haup-decomp-unit an
	  (when indirect-proof
	    (when r2ntop*trans-debug
	      (omega~message "~%~%~%r2ntop*trans-debug: Making the proof indirect.")
	      (omega~message "~%----------------------------------------------"))
	    (res2nd=indirect-decomposition-unit! (first start-units)))
	  
	  (when r2ntop*trans-debug
	    (omega~message "~%~%~%r2ntop*trans-debug: Starting the MAIN-LOOP.")
	    (omega~message "~%---------------------------------------------------------"))
	  
	  ;; Main procedure:
	  (res2nd=translate-ground-sspu-units
	   (res2nd=sspu-units-2-ground-sspu-units
	    (res2nd=arbitrary-units-2-sspu-units start-units nil)
	    nil)))
	
	))
    
    
    (when r2ntop*trans-debug
      (omega~message "~%~%~%r2ntop*trans-debug: Making some End-Settings.")
      (omega~message "~%---------------------------------------------"))
    
    ;; einige Anpassungen in den Justifications, reasons usw. -> Planer gedoens !
    (let* ((new-just (pdsj~set-below-justs r2ntop*conclusion-just
					   (list (node~justification (prob~proof-root omega*current-proof-plan))))))
      
      (setf (node~justification (prob~proof-root omega*current-proof-plan)) new-just)
      (when original-conc-node
	(setf (node~justification original-conc-node) new-just)))
    
    (tacl~end)
    
    (res2nd=check-tertium-non-datur)
    
    omega*current-proof-plan)
  )
|#

;; Neuer Version, eingefuehrt im Zuge der Lemmatisierung!
;; Man BEACHTE: IM MOMENT SIND IN DEN LEMMAS NUR SKOLEM-KONSTANTEN, KEINE FUNKTIONEN ERLAUBT !!!!!!!!!!!!!!!

(defun res2nd~transform (res-proof &key
				   (sspu-style 'aut)
				   (indirect-proof nil)
				   (maximal-depth 2)
				   (integral-formulas nil)
				   (reach-sspu-style 'case)
				   (avoid-doubeling nil)
				   (label-counter nil)
				   (original-conc-node nil)
				   (original-env nil)
				   (lemmas nil))
  (declare (edited  "25-MAR-1996")
	   (authors Ameier)
	   (input   "A resolution proof and by keywords a some settings for the transformation.")
	   (effect  "The resolution-proof is translated into an nd-proof, which is"
		    "inserted into omega*current-proof-plan and keim::pds*current-proof-plan.")
	   (value   "omega*current-proof-plan."))

  (res2nd=copy-reflex-clauses! res-proof)
  
  ;; Setting some global variables

  (setq r2ntop*lemma lemmas)
  ;; Schalter: mit oder ohne lemmas, welche Art von Lemmata erlaubt
  ;; falls nil       -> keine Lemmatisiserung
  ;; falls constants -> Lemmatisierung nur uber Lemmas mit Skolem-Konstanten, nicht uber Lemmas mit Skolem-Funktionen
  ;; sonst           -> volle Lemmatisierung
  
  (setq r2ntop*lemma*relative-weight 2) ;; -> spaeter anders !! Nach Eingabe !!
  
  (setq r2ntop*lemma-skolems nil)
  (setq r2ntop*lemma-nodes nil)
  (setq r2ntop*indirect-proof-forced nil)

  (setq r2ntop*sspu-style sspu-style)
  (setq integ*maximal-depth maximal-depth)
  (setq r2ntop*integral-formulas integral-formulas)
  (setq r2ntop*avoid-doubeling avoid-doubeling)
  (setq r2ntop*tertium-non-datur nil)
  (setq r2ntop*conclusion-just (if original-conc-node
				   (node~justification original-conc-node)
				 nil))
  (setq r2ntop*new-constants nil)
  (setq r2ntop*indirect-proof indirect-proof)

  (when r2ntop*trans-debug
    (omega~message "~%~%~%r2ntop*trans-debug: Creating the initial PDS.")
    (omega~message "~%---------------------------------------------"))
  
  ;; creates a new PDS with an open line that comes from the conlusion of the resolution proof and
  ;; hypothesis that come from the assumptions of the resolution proof
  ;; omega*current-proof-plan and keim::pds*current-proof-plan are set to this new PDS
  (res2nd=init-pds-from-res-proof res-proof
				  :label-counter label-counter
				  :original-env original-env)

  (when r2ntop*trans-debug
    (omega~message "~%~%~%r2ntop*trans-debug: Creating the Extdelta-Relation.")
    (omega~message "~%---------------------------------------------------"))
  
  (let* ((nodes (prob~proof-steps omega*current-proof-plan))
	 ;; erweitert delta-relation zur extdelta-relation
	 (extdelta-relation (extdelta~add-nodes-to-delta nodes (res~proof-delta-relation res-proof))))
    
    (multiple-value-bind
	(list-of-conclusions list-of-res-proofs list-of-extdeltas)
	;; zerlegt resolution proof durch Lemmatisierung etc. in kleinere Bestandteile -> kann, bzw. muss auch PDS veraendern und
	;; weitere offene Zeilen erzeugen, pro resolution proof eine
	;; Man beachte: An erster Stelle muss immer der Haupt-resolution bzw. Extdelta stehen, d.h. der, der die letztendlich
	;; conclusion schliesst ! -> Braucht man beim Aufruf indirekter Beweise !!
	(res2nd=split-res-proof-and-delta res-proof extdelta-relation)
      
      (when r2ntop*trans-debug
	(omega~message "~%~%~%r2ntop*trans-debug: Creating the Refutation Graphs.")
	(omega~message "~%---------------------------------------------------"))
      
      (let* ((list-of-ref-graphs (mapcar #'(lambda (res-proof extdelta-relation)
					     ;; berechnet refutation graph zu resolution proof und updated die extdelta-relation
					     (res2ref~get-refutation-graph-from-resolution-proof res-proof extdelta-relation))
					 list-of-res-proofs
					 list-of-extdeltas))
	     (indirect-conc-node
	      ;; Falls indirect-proof eingestellt oder erzwungen durch lemmatisierung
	      ;; -> passe conclusion-nodes und extdelta-relations an
	      (if (or indirect-proof r2ntop*indirect-proof-forced)
		  (progn (when r2ntop*trans-debug
			   (omega~message "~%~%~%r2ntop*trans-debug: Making the proof indirect.")
			   (omega~message "~%----------------------------------------------"))
			 (res2nd=make-indirect! (first list-of-conclusions) 
						list-of-extdeltas))
		nil)))

	;; PULLEN DER QUANTOREN -> VEERAENDERUNG DER EXDELTAS + PDS , falls Lemmatisiert wurde
	(when r2ntop*lemma
	  (lemma~pull-the-exists! list-of-extdeltas))
	
	(omega~message "~%~% Translating ...")
	
	;; Some more settings of global variables:
	(if (remove-if-not #'(lambda (clause)
			       (or (typep clause 'r2ntop+paramod-clause)
				   (typep clause 'r2ntop+reflex-clause)
				   (typep clause 'r2ntop+flip-clause)))
			   (apply 'append (mapcar #'(lambda (ref-graph)
						      (ref~clause-nodes ref-graph))
						  list-of-ref-graphs)))    
	    (setq r2ntop*reach-sspu-style 'case)
	  (setq r2ntop*reach-sspu-style reach-sspu-style))
	
	(when r2ntop*trans-debug
	  (omega~message "~%~%~%r2ntop*trans-debug: Creating the initial decompose units.")
	  (omega~message "~%---------------------------------------------------------"))
	
	;; berechnet die Start-decomposition Units
	(let* ((start-units (mapcar #'(lambda (conclusion-node refutation-graph extdelta-relation)
					(decomp~new-decompose-unit conclusion-node
								   refutation-graph
								   extdelta-relation))
				    (if (null indirect-conc-node)
					list-of-conclusions
				      (cons indirect-conc-node (rest list-of-conclusions)))
				    list-of-ref-graphs
				    list-of-extdeltas)))
	  
	  ;; Hier kommen die wirklich vorkommenden Skolem-Functionen rein, wirklich vorkommend heisst, solche, die
	  ;; wirklich nach Filterung, Optimierung usw. noch in Clauseln der Refutation-Graphs vorkommen, und daher auch
	  ;; mittels der entsprechenden extdelta-paare erreichbar sind 
	  ;; -> benutzt in Decompose und Link-ordering !!!
	  (setq r2ntop*skolem-constants
		(remove-duplicates
		 (apply 'append (mapcar #'res2nd=seek-skolem-constants start-units))))
	  ;; ^|koennte man jeweils auch pro refutation graph machen? Noe warum ?! ^|
	  
	  
	  (when r2ntop*trans-debug
	    (omega~message "~%~%~%r2ntop*trans-debug: Starting the MAIN-LOOP.")
	    (omega~message "~%---------------------------------------------------------"))

	  ;; lemma-pre-processing
	  (when r2ntop*lemma
	    ;; main-unit ist immer die erste der units

	    (when r2ntop*trans-debug
	      (omega~message "~%~%~%r2ntop*trans-debug: STARTING LEMMA-PREPROCESSING")
	      (omega~message "~%------------------------------------------------"))
	    (setq start-units (res2nd=lemma-pre-processing (list (first start-units)) (rest start-units))))
	  
	  ;; Main procedure:
	  (res2nd=translate-ground-sspu-units
	   (res2nd=sspu-units-2-ground-sspu-units
	    (res2nd=arbitrary-units-2-sspu-units start-units nil)
	    nil)))
	
	))
    
    
    (when r2ntop*trans-debug
      (omega~message "~%~%~%r2ntop*trans-debug: Making some End-Settings.")
      (omega~message "~%---------------------------------------------"))
    
    ;; einige Anpassungen in den Justifications, reasons usw. -> Planer gedoens !
    (let* ((new-just (pdsj~set-below-justs r2ntop*conclusion-just
					   (list (node~justification (prob~proof-root omega*current-proof-plan))))))
      
      (setf (node~justification (prob~proof-root omega*current-proof-plan)) new-just)
      (when original-conc-node
	(setf (node~justification original-conc-node) new-just)))
    
    (tacl~end)

    ;; remove any applications of same!
    (keim::pds=cleanup-same*! omega*current-proof-plan)

    ;; Check whether TND is used right
    (res2nd=check-tertium-non-datur)

    (omega~message "~% Translation finished!")
    
    omega*current-proof-plan)
  )



#| --------------------------------------------- ENDE HAUPTPROGRAMM --------------------------------------------------- |#

#| ----------------------------------------------- Initialization ----------------------------------------------------- |#


(defun res2nd=split-res-proof-and-delta (res-proof extdelta-relation)
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "A resolution proof and the according extdelta-relation.")
	   (effect  "None (oder was immer hier noch reinkommt.")
	   (value   "Multiple-value:"
		    "First: A list of resolution proofs"
		    "Second: A list of the according extdelta-relations."
		    "Third: A list of the according conclusion-nodes."))

  (if r2ntop*lemma
      
      (progn
	
	(when r2ntop*trans-debug
	  (omega~message "~%~%~%r2ntop*trans-debug: Seeking for Lemmas")
	  (omega~message "~%--------------------------------------"))
	
	
	(let* ((triple-list (lemma~split-res-proof! res-proof extdelta-relation (first (pds~open-nodes omega*current-proof-plan)))))
	  
	  ;; (setq global*res-proofs (mapcar #'first triple-list))
	  ;; (setq global*extdelta-relations (mapcar #'second triple-list))
	  ;; (setq global*conclusion-nodes (mapcar #'third triple-list))
	  ;; (error "KJGJKHG")
	  
	  (values (mapcar #'third triple-list)
		  (mapcar #'first triple-list)
		  (mapcar #'second triple-list)
		  )))

    (values (list (first (pds~open-nodes omega*current-proof-plan)))
	    (list res-proof)
	    (list extdelta-relation))))

(defun res2nd=make-indirect! (conclusion-node list-of-extdeltas)
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "The main-conclusion-node and the list of extdelta-relations.")
	   (effect  "The PDS proof is styled indirect. This means that the main-conclusion-node of"
		    "is inserted negated into the PDS and the conclusion is shown indirectly."
		    "The extdelta-relations are changed by this operation by replacing occurrences of"
		    "the main-conclusion-node by its new negated node."
		    "Additionally all lemma-node (in r2ntop*lemma-nodes) get also as new-hypothesis the negated node.")
	   (value   "The new main-conclusion-node: false."))
  
  (if (logic~negation-p (node~formula conclusion-node))
      
      ;; indirekter Beweis mittels Regel 'noti erzeugt
      (let* ((new-nodes (r2ntop~apply-tactic 'noti (list conclusion-node nil) nil))
	     (unnegated-conclusion-node (third new-nodes))
	     (new-conclusion-node (second new-nodes)))

	;; aus conclusion-node (not F) wird unnegated-conclusion-node F -> aus position (1 Rest) wird (Rest)
	;; updaten der extdelta-relations
	
	(mapcar #'(lambda (extdelta-relation)
		    (let* ((pairs (extdelta~relation-pairs extdelta-relation)))
		      (mapcar #'(lambda (pair)
				  (let* ((node (extdelta~pdsnode pair)))
				    (when (eq node conclusion-node)
				      (setf (extdelta~delta-node-of-formula pair) unnegated-conclusion-node)
				      (setf (delta~delta-position-in-formula pair) (pos~rest (extdelta~formula-position pair)))
				      (setf (delta~delta-formula pair) (termix~create-named-term
									(keim~name unnegated-conclusion-node)
									(data~copy (node~formula unnegated-conclusion-node)
										   :downto '(data+primitive)))))))
			      pairs)))
		list-of-extdeltas)
	
	;; updaten der Hypothesis der lemma-nodes: immer unnegated-conclusion-node hinzu + (falls drin) conclusion-node raus
	(mapcar #'(lambda (lemma-node)
		    (setf (pdsn~hyps lemma-node)
			  (cons unnegated-conclusion-node
				(remove conclusion-node (pdsn~hyps lemma-node)))))
		r2ntop*lemma-nodes)
	
	new-conclusion-node)
    
    ;; indirekter Beweis mittels Regel 'indirect erzeugt
    (let* ((new-nodes (r2ntop~apply-tactic 'indirect (list conclusion-node nil) nil))
	   (negated-conclusion-node (third new-nodes))
	   (new-conclusion-node (second new-nodes)))

      ;; aus conclusion-node F wird negated-conclusion-node (not F) -> aus position (Rest) wird (1 Rest)
      ;; updaten der extdelta-relations

      (mapcar #'(lambda (extdelta-relation)
		    (let* ((pairs (extdelta~relation-pairs extdelta-relation)))
		      (mapcar #'(lambda (pair)
				  (let* ((node (extdelta~pdsnode pair)))
				    (when (eq node conclusion-node)
				      (setf (extdelta~delta-node-of-formula pair) negated-conclusion-node)
				      (setf (delta~delta-position-in-formula pair) (pos~add-front '1 (extdelta~formula-position pair)))
				      (setf (delta~delta-formula pair) (termix~create-named-term
									(keim~name negated-conclusion-node)
									(data~copy (node~formula negated-conclusion-node)
										   :downto '(data+primitive)))))))
			      pairs)))
	      list-of-extdeltas)

      ;; updaten der Hypothesis der lemma-nodes: immer negated-conclusion-node hinzu + (falls drin) conclusion-node raus
      (mapcar #'(lambda (lemma-node)
		  (setf (pdsn~hyps lemma-node)
			(cons negated-conclusion-node
			      (remove conclusion-node (pdsn~hyps lemma-node)))))
	      r2ntop*lemma-nodes)
      
      new-conclusion-node)))


(defun res2nd=indirect-decomposition-unit! (unit)
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "A decompose-unit.")
	   (effect  "The PDS proof is styled indirect. This means that the conclusion-node of"
		    "the unit is inserted negated and the conclusion is shown indirectly."
		    "The extdelta-relation of the unit is also changed by this operation from"
		    "the conclusion node to the negated node.")
	   (value   "Undefined."))
  (let* ((conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	 (nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit)))
    (if (logic~negation-p (node~formula conclusion-node))
	(let* ((new-nodes (r2ntop~apply-tactic 'noti (list conclusion-node nil) nil))
	       (unnegated-conclusion-node (third new-nodes))
	       (new-conclusion-node (second new-nodes)))
	  
	  ;; update delta-relation + nodes-to-decompose + integral-formulas + conclusion-node
	  (when (not (find conclusion-node nodes-to-decompose))
	    (setf (r2ntop~decompose-unit-nodes-to-decompose unit)
		  (cons conclusion-node nodes-to-decompose))) 
	  ;; updaten der conclusion-node in nodes-to-decompose |^

	  ;; update conclusion-node
	  (setf (r2ntop~decompose-unit-conclusion-node unit)
		new-conclusion-node)
	  
	  (decomp~update-unit unit conclusion-node 
			      (list (list unnegated-conclusion-node (list (list (pos~list-position '(1))
										(pos~list-position '())))))
			      nil (list unnegated-conclusion-node)))
      
      (let* ((new-nodes (r2ntop~apply-tactic 'indirect (list conclusion-node nil) nil))
	     (negated-conclusion-node (third new-nodes))
	     (new-conclusion-node (second new-nodes)))
	
	;; update delta-relation + nodes-to-decompose + integral-formuls
	(when (not (member conclusion-node nodes-to-decompose))
	  (setf (r2ntop~decompose-unit-nodes-to-decompose unit)
		(cons conclusion-node nodes-to-decompose))) 

	;; update conclusion-node
	(setf (r2ntop~decompose-unit-conclusion-node unit)
	      new-conclusion-node)
	
	(decomp~update-unit unit conclusion-node
			    (list (list negated-conclusion-node (list (list (pos~list-position '())
									    (pos~list-position '(1))))))
			    nil (list negated-conclusion-node))))))


(defun res2nd=seek-skolem-constants (unit) 
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "A decomposition unit.")
	   (effect  "None.")
	   (value   "A list of skolem-constants."
		    "The list contains the skolem-constants, that are in the CNF literals of the"
		    "nodes, that are connected by the delta-relation with the refutation graph of"
		    "the input unit, but only those skolem-constants, that appear in literals, that"
		    "belong to delta-pairs from the delta-relation of the input unit." ))
  (let* ((delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	 (conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	 (pairs (extdelta~relation-pairs delta-relation))
	 (nodes (remove-duplicates (mapcar #'extdelta~delta-node-of-formula pairs))))
    (remove-duplicates
     (apply 'append
	    (mapcar #'(lambda (node)
			(let (;; sucht pairs zu diesem Knoten heraus
			      (pairs-of-node (remove-if-not #'(lambda (pair)
								(eq (extdelta~pdsnode pair) node))
							    pairs))
			      ;; setzt flag, ob Knoten geschlossene Zeile (modulo dieser UNIT !)
			      (flag (if (eq conclusion-node node) 
					nil
				      't)))
			  (remove-duplicates (res2nd=get-skolems-of-formula (node~formula node)
									    (pos~list-position ())
									    pairs-of-node
									    ()
									    flag))))
		    nodes)))))

(defun res2nd=get-skolems-of-formula (formula current-pos-prefix according-delta-pairs existential-variable-list flag)
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "A formula, a position prefix (that sign the position of the current formula in a bigger"
		    "origin formula, a list of delta-pairs, a list of existential-variables (in whose scope"
		    "the current formula is, if you see it as a part of the bigger origin formula) and a flag"
		    "that sign wether the polarity of the current formula (looked at from the bigger origin"
		    "formula).")
	   (effect  "None.")
	   (value   "A list of skolem-functions."
		    "The list contains this skolem-functions, that appear in the literals of the CNF of the formula"
		    "but only those literals, that really belong to delta-pairs of the input delta-pair-list."))
  (cond ((logic~universal-quantification-p formula)
	 (let*  ((bound-term (first (data~appl-arguments formula)))
		 (bound-variable (first (data~abstr-domain bound-term))))
	   (if (null flag)
	       ;; -> planned-node or negated -> forall variable is really a existential variable
	       (res2nd=get-skolems-of-formula (data~abstr-range bound-term)
					      (pos~add-end 0 (pos~add-end 1 current-pos-prefix))
					      according-delta-pairs
					      (cons bound-variable (remove bound-variable existential-variable-list))
					      ;; cons - remove -> is wohl Schwachsinn aber lassen wirs mal drin ...
					      flag)
	     ;; -> really a forall variable
	     (res2nd=get-skolems-of-formula (data~abstr-scope bound-term)
					    (pos~add-end 0 (pos~add-end 1 current-pos-prefix))
					    according-delta-pairs
					    (remove bound-variable existential-variable-list)
					    ;; till now all occurences of the bound-variable accord to this
					    ;; forall quantification -> remove bound-variable from the list of
					    ;; existential-variables
					    ;; -> is wohl Schwachsinn aber lassen wirs maldrin ...
					    flag))))
	((logic~existential-quantification-p formula)     
	 (let*  ((bound-term (first (data~appl-arguments formula)))
		 (bound-variable (first (data~abstr-domain bound-term))))
	   (if flag
	       ;; -> really a exists variable
	       (res2nd=get-skolems-of-formula (data~abstr-scope bound-term)
					      (pos~add-end 0 (pos~add-end 1 current-pos-prefix))
					      according-delta-pairs
					      (cons bound-variable (remove bound-variable existential-variable-list))
					      ;; cons - remove -> is wohl Schwachsinn aber lassen wirs mal drin ...
					      flag)
	     ;; -> planned-node or negated -> exists variable is really a forall variablew-
	     (res2nd=get-skolems-of-formula (data~abstr-scope bound-term)
					    (pos~add-end 0 (pos~add-end 1 current-pos-prefix))
					    according-delta-pairs
					    (remove bound-variable existential-variable-list)
					    ;; till now all occurences of the bound-variable accord to this
					    ;; forall quantification -> remove bound-variable from the list of
					    ;; existential-variables
					    ;; -> is wohl Schwachsinn aber lassen wirs mal drin ...
					    flag))))
	((logic~implication-p formula)
	 (append
	  (res2nd=get-skolems-of-formula (first (data~appl-arguments formula))
					 (pos~add-end 1 current-pos-prefix)
					 according-delta-pairs
					 existential-variable-list
					 (null flag))
	  (res2nd=get-skolems-of-formula (second (data~appl-arguments formula))
					 (pos~add-end 2 current-pos-prefix)
					 according-delta-pairs
					 existential-variable-list
					 flag)))
	((or (logic~conjunction-p formula) (logic~disjunction-p formula))
	 (append
	  (res2nd=get-skolems-of-formula (first (data~appl-arguments formula))
					 (pos~add-end 1 current-pos-prefix)
					 according-delta-pairs
					 existential-variable-list
					 flag)
	  (res2nd=get-skolems-of-formula (second (data~appl-arguments formula))
					 (pos~add-end 2 current-pos-prefix)
					 according-delta-pairs
					 existential-variable-list
					 flag)))
	((logic~negation-p formula)
	 (res2nd=get-skolems-of-formula (first (data~appl-arguments formula))
					(pos~add-end 1 current-pos-prefix)
					according-delta-pairs
					existential-variable-list
					(null flag)))
	((logic~equivalence-p formula)
	 (let* ((implies (env~lookup-object 'implies (pds~environment omega*current-proof-plan)))
		(and (env~lookup-object 'and (pds~environment omega*current-proof-plan)))
		(arguments (data~appl-arguments formula))
		(and-formula (term~appl-create and (list (term~appl-create implies arguments)
							 (term~appl-create implies (reverse arguments))))))
	   (res2nd=get-skolems-of-formula and-formula
					  current-pos-prefix
					  according-delta-pairs
					  existential-variable-list
					  flag)))
	(t      ;; -> literal-term

	 ;; formula                   -> literale Formel
	 ;; current-pos-prefix        -> die Position dieser literalen Formel in ihrer urspruenglichen Oberformel
	 ;; according-delta-pairs     -> Alle Delta-paare dieser urspruenglichen Formel
	 ;; existential-variable-list -> alle existential- (oder besser nach Fitting delta-) qunatifizierten Variablen
	 ;;                           -> der uspruenglichen Formel auf dem Pfad zu diesem Literal
	 
	 (apply 'append
		(mapcar #'(lambda (ex-variable)
			    ;; Positionen der existenz-quantifizierten Variable im literalen Subterm
			    (let ((positions-of-ex-var-in-term (data~substruct-positions ex-variable formula)))
			      (when positions-of-ex-var-in-term
				(let* (;; delta-paare mit der Position der literlen Formel, d.h. die auf literale Formel zeigen
				       (pairs-with-this-position
					(remove-if-not #'(lambda (pair)
							   (keim~equal current-pos-prefix (extdelta~formula-position pair)))
						       according-delta-pairs))
				       ;; zugehoerige Terme aus den Literalen der Klauseln (-> CNF -> Skolemterme drin !)
				       (according-terms
					(mapcar #'(lambda (pair)
						    (let ((clause (extdelta~clause pair))
							  (clause-position (extdelta~clause-position pair)))
						      (lit~atom (r2ntop~term-at-position (cl~literals clause) clause-position))))
						pairs-with-this-position))
				       ;; Die Skolemterme aus den Literalen
				       (skolem-terms (mapcar #'(lambda (term)
								 (r2ntop~term-at-position term (first positions-of-ex-var-in-term)))
							     according-terms)))

				  ;; Rueckgabe: Die Skolemfunctionen:
				  (mapcar #'(lambda (skolem-term)
					      (if (data~appl-p skolem-term)
						  (data~appl-function skolem-term)
						skolem-term))
					  skolem-terms)))))
			existential-variable-list)))))

(defun res2nd=init-pds-from-res-proof (res-proof &key (label-counter nil) (original-env nil))
  (declare (edited  "04-MAR-1996")
	   (authors Ameier)
	   (input   "A resolution-proof and a list of labels of assumptions from"
		    "the resolution-proof.")
	   (effect  "The corresponding initial PDS is created, with the conclusion as planned-node"
		    "and the assumption as hypothesis and supports of the planned-node."
		    "omega*current-proof-plan and keim::pds*current-proof-plan are set to this new"
		    "pds.")
	   (value   "The new PDS (EQ to omega*current-proof-plan and keim::pds*current-proof-plan"))
  
  ;; create a new PDS and new nodes
  (let* ((name (keim~name res-proof))
	 (env (res~proof-environment res-proof))
	 (new-env (if original-env
		      (env~create (list env original-env))
		    (env~create env)))
	 (assumptions (res~proof-assumptions res-proof))
	 (conclusion (res~proof-conclusion res-proof))
	 ;; (ass-names (mapcar #'keim~name assumptions))
	 ;; (conc-name (keim~name conclusion))
	 (new-pds (pds~proof-plan-create (intern (string-upcase (format nil "~A-pds" name)) (find-package :omega))))
	 (hyp-nodes (mapcar #'(lambda (ass)
				(let* ((new-hyp-node (pdsn~make-hypothesis (termix~term ass) (keim~name ass))))
				  (keim~put new-hyp-node 'source ass)
				  new-hyp-node))
			    assumptions))
	 (conc-node (pdsn~open-node-create (termix~term conclusion) hyp-nodes (keim~name conclusion))))

    ;; set the keim::pds*current-proof-plan and omega*current-proof-plan to this new pds
    (setq keim::pds*current-proof-plan new-pds)
    (setq omega*current-proof-plan new-pds)
    (setf (pds~open-nodes new-pds) nil) ;; setzt den slot open nodes vorlaeufig

    ;; wenn urpruenglicher Knoten bereits justifiziert (Otter, Protein usw.) nimm diese Justification fuer die neue Conclusion
    ;; und passe sie an:
    ;; Set the justification of conc-node to the justification of the original conclusion node
    ;; and replace in this justification all premises-nodes (from the original proof) by the same named
    ;; nodes in the new proof (this replacement is turned back in the otter~call-otter function)
    (when r2ntop*conclusion-just
      (setf (node~justification conc-node) r2ntop*conclusion-just)
      (setf (just~premises r2ntop*conclusion-just) hyp-nodes))

    ;; gibt es keine urspeungliche Justification, z.b. transform resolution proof allein, dann erzeuge einfach (defaultmaessig)
    ;; eine Otter-Justification
    (when (null r2ntop*conclusion-just)
      ;; justify the conclusion by otter
      (infer~compute-outline (infer~find-method 'otter) (cons conc-node hyp-nodes) (list nil))
      
      ;;      (setf (pds~open-nodes omega*current-proof-plan) (remove conc-node (pds~open-nodes omega*current-proof-plan)))
      ;;      
      ;;      ;; the supports and the conclusion get this otter as reason
      ;;      (let* ((otter-reason (pds~change-last-plan-step! conc-node)))
      ;;        (mapcar #'(lambda (node)                                    ;; Rausschmeissen HACK AMEIER
      ;;                    (pdsn~insert-reason! node otter-reason))
      ;;                (cons conc-node hyp-nodes)))
      (setq r2ntop*conclusion-just (node~justification conc-node)))
    
    (keim~put conc-node 'source conclusion)
    
    ;; setzt Label Counter (falls dieser bereits von oben kam ...
    (when label-counter
      (setf (keim::pds=label-counter new-pds) label-counter))

    ;; schreibt hyps und conclusion in die neue PDS und setzt einige andere Sachen ...
    (mapcar #'(lambda (x)
		(pds~only-insert-node! x new-pds))
	    hyp-nodes)
    (setf (pds~open-nodes new-pds) (list conc-node))
    (setf (pds~support-nodes new-pds) hyp-nodes)
    ;; insert last-line conc-node ??
    (setf (prob~proof-root new-pds) conc-node)
    (pds~only-insert-node! conc-node new-pds)
    (setf (pds~environment new-pds) new-env)
    (setf (prob~proof-theory new-pds) (prob~proof-theory res-proof))
    (setf (prob~proof-problem new-pds) (if (prob~p (prob~proof-problem res-proof))
					   (prob~proof-problem res-proof)  ;; falls direkt aus Problem
					 (prob~proof-problem (prob~proof-problem res-proof)))) ;; falls aus PDS
    (setf (prob~proof-steps new-pds) (append hyp-nodes (list conc-node)))
    
    ;; this tacl~init deletes the r2ntop*conclusion-just and makes the conc-node again to an open node
    (tacl~init (cons conc-node hyp-nodes))
    
    new-pds
    ))

;; Schwachsinn: Rausschmeissen: Hack AMEIER
;;    (res2nd=init-ndproof! res-proof conc-name ass-names)))
;;
;;
;;(defun res2nd=init-ndproof! (res-proof conclusion-label premises-labels)
;;  (declare (edited  "31-JAN-1995")
;;           (authors Ameier)
;;           (input  "A RESOLUTION-PROOF."  )
;;           (effect "The ND nodes containing the conclusion, the premises and the assertions"
;;                   "must interactively be supplied by the user. The"
;;                   "conclusion-node is then justified by the given premises, assertions,"
;;                   "RESOLUTION-PROOF, and an extended delta relation which is computed."
;;                   "from the delta-relation of RESOLUTION-PROOF." )
;;           (value  "The altered ND proof." ))
;;  (let* ((delta (res~proof-delta-relation res-proof))
;;         (conclusion (pds~label2node conclusion-label))
;;         (premises (mapcar #'pds~label2node premises-labels))
;;         (extdelta-relation (extdelta~add-nodes-to-delta (prob~proof-steps ndprf) delta)))
;;    (if (pdsn~open-node-p conclusion)
;;        (setf (node~justification conclusion)
;;              (r2ntop~create-justification
;;               nil
;;               premises
;;               conclusion
;;               nil
;;               extdelta-relation))
;;      (res2nd~error "The ND node for conclusion ~A is not a planned node!" conclusion))
;;    ndprf))
;;

#| ------------------------------------------ END INITIALIZIERUNG ------------------------------------------------------ |#

#| --------------------------------------------------- Lemma Preprocessing --------------------------------------------- |#

(defun res2nd=lemma-pre-processing (main-units lemma-units)
  (declare (edited  "18-JAN-1999")
	   (authors Ameier)
	   (input   "The main units and the lemma-units.") ;; gibts es uberhaupt mehr als ein main unit ?
	   (effect  "The lemmas can still contain skolem-constants and dummy nodes. This function computes"
		    "all nodes to such skolem-constants and (the according exists are already pulled in front)"
		    "and apply existse (choice) to them until all skolem-constants are introduced and all"
		    "dummy lines are eliminated.")
	   (value   "The list of all translation units to start with the standard translation."))
  (let* (;; die momentanen Lemma-skolems stehen immer in r2ntop*lemma-skolems
	 ;; alle lemma-nodes stehen in r2ntop*lemma-nodes
	 (nodes-to-lemma-skolems (remove-duplicates
				  (mapcar #'(lambda (skolem)
					      (res2nd=get-skolem-node skolem
								      (mapcar #'r2ntop~decompose-unit-extdelta-relation
									      (append main-units lemma-units))
								      (append r2ntop*lemma-nodes
									      (mapcar #'r2ntop~decompose-unit-conclusion-node
										      lemma-units))))
					  
					  r2ntop*lemma-skolems))))
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: LEMMA-PREPROCESSING, LEMMA-SKOLEMS: ~A, NODES-TO-SKOLEMS: ~A"
		     r2ntop*lemma-skolems
		     nodes-to-lemma-skolems))
    
    (if (null nodes-to-lemma-skolems)
	(progn

	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: FINISHING LEMMA-PREPROCESSING"))
	  
	  ;; Am SCHLUSS: Lemmatisierung beedendet:
	  (setq r2ntop*lemma nil)
	  
	  (append main-units lemma-units))
      
      (let* ((next-node-to-decompose (first nodes-to-lemma-skolems)))
	
	;; formel muss existential quantifiziert sein
	
	(res2nd=decomposing-exists-formulas-during-lemma next-node-to-decompose main-units lemma-units)
	
	(res2nd=lemma-pre-processing main-units lemma-units)))))


(defun res2nd=decomposing-exists-formulas-during-lemma (exists-node main-units lemma-units)
  (declare (edited  "14-MAY-1998")
	   (authors Ameier)
	   (input   "A (not open) existential quantified node, a list of main-units and a list of"
		    "lemma-units.") 
	   (effect  "Seeks wether the exists-node accords to a skolem in r2ntop*lemma-skolems."
		    "If this is not the case, the node will be decomposed normally over units,"
		    "If it is the case, the according skolem is removed from r2ntop*lemma-skolems"
		    "and the exists-node is decomposed not only over all units, that have this"
		    "node in the nodes to decompose, but also over all main-units, that use lemmata"
		    "that contain this skolem.")
	   (value   "Undefined."))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Decomposing exists-node ~A"
		   exists-node
		   ))
  
  (let* ((extdelta-relations (mapcar #'r2ntop~decompose-unit-extdelta-relation (append main-units lemma-units)))
	 (skolem-term (first (remove-duplicates (apply 'append
						       (mapcar #'(lambda (extdelta)
								   (decomp~all-subst-terms-of-quantified-var exists-node extdelta
													     :kind 'delta))
							       extdelta-relations)))))
	 (dummy-planned-node (pdsn~open-node-create (term~constant-create 'a (type~o)) (pdsn~hyps exists-node) (gensym "dummy-open")))
	 (new-constant (r2ntop~new-const (first (data~abstr-domain
						 (first (data~appl-arguments (node~formula exists-node)))))))
	 (new-nodes (r2ntop~apply-tactic 'existse (list dummy-planned-node exists-node nil) (list new-constant)))
	 (hyp-node (fourth new-nodes)))

    ;; entferne new-theorem-node:
    (keim::pds=remove-node! omega*current-proof-plan (third new-nodes) nil t)
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: Found according skolem-term: ~A"
		     skolem-term))
    
    
    (if (or (null skolem-term) (not (find skolem-term r2ntop*lemma-skolems)))
	
	(progn

	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: ~A is NOT in the r2ntop*lemma-skolems: ~A -> standart decomposition"
			   skolem-term
			   r2ntop*lemma-skolems))
	  
	  (mapcar #'(lambda (unit)
		      (res2nd=existse-over-units unit exists-node hyp-node new-constant))
		  (append main-units lemma-units)))

      ;; SKOLEM-TERM zu dieser exists-node gehoert zu lemmata ->
      ;; 1. Ersetze in allen Refutation Grpahs den SKOLEM-TERM durch die neue Konstante
      ;; 2. wende existse ueber alle main-units an, die Lemmata enthalten, die imn ihren HYPS! dummy-hyps haben, mit der Skolem-Konstante
      ;;    als Formel! (Die Abfrage muss ueber die HYPS gehen, nicht uber die FOrmel der Lemmata, wegen verschachtelter Lemmata !)
      ;; 3. wende existse ueber alle lemma-units an, die es in den nodes-to-decompose haben, und deren Conclusion-node KEINE dummy-hyp
      ;;    enthaelt mit dem Skolem-term als Formel (sonst haengen sie sowieso schon davon ab).
      ;; 4. Alle lemma-nodes, die die exists-node in ihren nodes-to-decompose haben, und deren conclusion-node EINE dummy-hyp enthaelt
      ;;    mit der Skolem-Konstante als Formel werden so geupdated, dass in ihren delta-relations die exists-node durch die hyp-node
      ;;    ersetzt wird.
      ;; 5. Ersetze ueber alle Knoten den Skolem-term durch die neue Konstante
      ;; 6. Ersetze in allen Knoten, dummy-hyps mit der Skolem-Konstante als Formel durch die neue Hyp
      ;; 7. Loesche den skolem-term aus r2ntop*lemma-skolems

      
      (progn

	(when r2ntop*trans-debug
	  (omega~message "~%~%r2ntop*trans-debug: ~A IS in the r2ntop*lemma-skolems: ~A -> extended decomposition"
			 skolem-term
			 r2ntop*lemma-skolems))
	
	
	(let* ((main-units-using-skolem-in-lemmata (remove-if-not #'(lambda (main-unit)
								      (res2nd=main-unit-contains-skolem-in-lemmata-p main-unit skolem-term))
								  main-units))
	       (lemma-units-to-decompose
		(remove-if-not #'(lambda (lemma-unit)
				   (let* ((hyps-of-conclusion (pdsn~hyps (r2ntop~decompose-unit-conclusion-node lemma-unit))))
				     (and (find exists-node (r2ntop~decompose-unit-nodes-to-decompose lemma-unit))
					  (not (find skolem-term hyps-of-conclusion
						     :test #'(lambda (skolem node)
							       (eq skolem (node~formula node))))))))
			       lemma-units))
	       (lemma-units-to-update
		(remove-if-not #'(lambda (lemma-unit)
				   (let* ((hyps-of-conclusion (pdsn~hyps (r2ntop~decompose-unit-conclusion-node lemma-unit))))
				     (and (find exists-node (r2ntop~decompose-unit-nodes-to-decompose lemma-unit))
					  (find skolem-term hyps-of-conclusion
						:test #'(lambda (skolem node)
							  (eq skolem (node~formula node)))))))
			       lemma-units))
	       )
	  
	  (mapcar #'(lambda (unit)
		      (decomp=replace-skolem-function-with-constant! new-constant
								     skolem-term
								     (r2ntop~decompose-unit-ref-graph unit))
		      )
		  (append main-units lemma-units))

	  (mapcar #'(lambda (unit)
		      (res2nd=existse-over-units unit exists-node hyp-node new-constant :main-unit 't))
		  main-units-using-skolem-in-lemmata)
	  
	  (mapcar #'(lambda (unit)
		      (res2nd=existse-over-units unit exists-node hyp-node new-constant))
		  lemma-units-to-decompose)
	  
	  (mapcar #'(lambda (unit)
		      ;; update delta-relation 
		      (decomp~update-unit unit exists-node
					  (list (list hyp-node (list (list (pos~list-position '(1 0))
									   (pos~list-position '())))))
					  (list exists-node) (list hyp-node)))
		  lemma-units-to-update)
	  
	  (setq r2ntop*lemma-skolems (remove skolem-term r2ntop*lemma-skolems))
	  
	  (mapcar #'(lambda (node)
		      (when (find skolem-term (pdsn~hyps node)
				  :test #'(lambda (term node)
					    (keim~equal term (node~formula node))))
			(setf (pdsn~hyps node)
			      (cons hyp-node (remove-if #'(lambda (node)
							    (keim~equal skolem-term (node~formula node)))
							(pdsn~hyps node)))))
		      (setf (node~formula node)
			    (data~replace-struct (node~formula node) skolem-term new-constant :destructive 't)))
		  (prob~proof-steps omega*current-proof-plan)))))))


(defun res2nd=main-unit-contains-skolem-in-lemmata-p (unit skolem)
  (declare (edited  "14-MAY-1998")
	   (authors Ameier)
	   (input   "A (main) decompostion unit and a skolem-Konstant.")
	   (effect  "None.")
	   (value   "T if the unit uses a lemma, that contains in its hyp-nodes"
		    "a dummy-hyp with the skolem-konstant as formula."))
  (let* ((extdelta (r2ntop~decompose-unit-extdelta-relation unit))
	 (nodes-of-unit (remove-duplicates (mapcar #'extdelta~pdsnode (delta~relation-pairs extdelta))))
	 (lemma-nodes-of-unit (remove-if-not #'(lambda (node)
						 (keim~get node 'lemma))
					     nodes-of-unit))
	 (hyps-of-lemma-nodes (apply 'append (mapcar #'pdsn~hyps lemma-nodes-of-unit))))
    
    (if (find skolem hyps-of-lemma-nodes
	      :test #'(lambda (skolem node)
			(eq skolem (node~formula node))))
	't
      nil)))


(defun res2nd=existse-over-units (unit existential-node hyp-node new-constant &key (main-unit nil))
  (declare (edited  "07-MAY-1998")
	   (authors Ameier)
	   (input   "A decomposition unit, an existential-node, the according hyp-node to the"
		    "existential node and a constant.")
	   (effect  "If the existential-node is contained in the set of nodes-to-decompose of"
		    "the unit and is not the conclusion node of the unit (or keyword main-unit"
		    "is true), existse is applied"
		    "on the existential-node, the hyp-node and the conclusion-node of the unit,"
		    "using the constant as parameter."
		    "This application will also cause some chenges in the delta-relation,the"
		    "nodes-to-decompose and the refutation-graph of the unit.")
	   (value   "Undefined."))
  (let* ((nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit))
	 (conclusion-node (r2ntop~decompose-unit-conclusion-node unit)))
    
    (when (or (and (not (eq conclusion-node existential-node))
		   (find existential-node nodes-to-decompose))
	      main-unit)
      (let* ((new-nodes (r2ntop~apply-tactic 'existse (list conclusion-node existential-node nil) (list new-constant)))
	     (new-hyp-node (fourth new-nodes))
	     (new-planned-node (third new-nodes))
	     (old-thm-node (first new-nodes))
	     )
	
	;; update conclusion-node
	(setf (r2ntop~decompose-unit-conclusion-node unit) new-planned-node)
	
	;; update delta-relation + nodes-to-decompose + integral-formulas
	(decomp~update-unit unit existential-node
			    (list (list hyp-node (list (list (pos~list-position '(1 0))
							     (pos~list-position '())))))
			    (list existential-node) (list hyp-node))
	(decomp~update-unit unit old-thm-node
			    (list (list new-planned-node (list (list (pos~list-position '())
								     (pos~list-position '())))))
			    nil nil)

	;; ersetze in den HYPS der neuen open node die new-hyp-node durch hyp-node
	(setf (pdsn~hyps new-planned-node)
	      (cons hyp-node (remove new-hyp-node (pdsn~hyps new-planned-node))))
	
	;; Loesche die neue Hyp-node
	(keim::pds=remove-node! omega*current-proof-plan new-hyp-node nil t)
	))))

(defun res2nd=get-skolem-node (skolem extdelta-relations lemma-nodes)
  (declare (edited  "13-MAY-1998")
	   (authors Ameier)
	   (input   "A skolem-constant, a list of extdelta-relations and a list"
		    "of lemma-nodes.")
	   (effect  "None.")
	   (value   "The node to a clause, that contain the skolem-constant, but is not in the"
		    "list of lemma-nodes."))
  (let* ((pairs (apply 'append (mapcar #'delta~relation-pairs extdelta-relations))))

    (extdelta~pdsnode (find skolem pairs
			    :test #'(lambda (skolem pair)
				      (and (not (find (extdelta~pdsnode pair) lemma-nodes))
					   (find skolem (lemma=skolems-of-clause (extdelta~clause pair)))))))))

#| ------------------------------------ SOME CHECK-FUNCTIONS FOR DECOMPOSITIONS ---------------------------------------- |#

(defun res2nd=forall-node-can-be-decomposed-p (forall-node delta-relation)
  (declare (edited  "16-APR-1996")
	   (authors Ameier)
	   (input   "A forall-node and the delta-relation.")
	   (effect  "None.")
	   (value   "T if at least one of the necessary forall-instantiations"
		    "does not contain a skolem-term, which means that this"
		    "instantiation could be immediately performed. An instantiation containing"
		    "a skolem-term could be performed after replacing the skolem"
		    "constant."
		    "Nil if no such instantiation exists."))
  (let ((subst-terms (decomp~all-subst-terms-of-quantified-var forall-node delta-relation :kind 'gamma)))
    (if subst-terms
	(do* ((terms subst-terms (rest terms))
	      (flag nil))
	    ((or (null terms) flag) flag)
	  (let ((term (first terms)))
	    (setq flag (null (data~positions term #'sksym~p)))))
      't)))
;; can be decomposed wenn es ueberhaupt eine skolemfrei Einsetzung gibt !!

#| ------------------------------------------------ Main loop ---------------------------------------------------------- |#

#|

Momentan ist folgendes implementiert:

arbitrary-units-2-sspu-units:
Es werden solange Zeilen decomponiert, bis die Units keine gegeneinander gerichteten Polylinks mehr enthalten.
Dazu werden zuerst die nodes-to-decompose benutzt (Nodes zu unit-clauses oder existentielle Subformeln enthaltende), da die
sowieso decomposed werden muessen. Erst wenn keine nodes-to-decompose mehr da sind werden (geschickt !!!) andere Zeilen
zum decomponieren ausgewaehlt und zwar solche, die garantiert contradictional Pairs aufheben.


sspu-units-2-ground-sspu-units:

Nun werden noch die restlichen nodes-to-decompose zerlegt und eventuell noch weitere um bestimmte Effekte zu erhalten
(z.b. moeglichst direkte Beweise).

translate-ground-sspu-units:

Nun werden die uebrig gebliebenen Sachen mittels Folgen von assertion Anwendungen uebersetzt.

|#

#| ------------------------------------------ ARBITRARY-UNIT -> SSPU-UNIT ---------------------------------------------- |#

(defun res2nd=arbitrary-units-2-sspu-units (list-of-non-sspu-units list-of-sspu-units)

  (when r2ntop*trans-debug
    (omega~message "~%~%~%r2ntop*trans-debug: arbitrary-units-2-sspu-units, with the following units:")
    (omega~message "~%---------------------------------------------------------------------------")
    (omega~message "~%The non-sspu-units:")
    (mapcar #'(lambda (unit)
		(omega~message "~%~A" unit))
	    list-of-non-sspu-units)
    (omega~message "~%The sspu-units:")
    (mapcar #'(lambda (unit)
		(omega~message "~%~A" unit))
	    list-of-sspu-units)
    )
  
  (if list-of-non-sspu-units
      (let* ((unit (first list-of-non-sspu-units))
	     (delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	     (conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	     (integ-check (integ~check-decompose-unit! unit)))
	(if integ-check
	    (progn

	      (when r2ntop*trans-debug
		(omega~message "~%~%r2ntop*trans-debug: integ-check suceeds on unit ~A" unit))
	      
	      ;; -> conclusion erschlagen durch integral-formeln -> unit ist bereits fertig
	      (res2nd=arbitrary-units-2-sspu-units (rest list-of-non-sspu-units) list-of-sspu-units))
	  
	  (progn
	    
	    (when r2ntop*trans-debug
	      (omega~message "~%~%r2ntop*trans-debug: integ-check fails on unit ~A" unit))
	    
	    (if (plco~sspu-refutable-p (r2ntop~decompose-unit-plco-pairs unit))
		
		(progn
		  (when r2ntop*trans-debug
		    (omega~message "~%~%r2ntop*trans-debug: sspu-refutable-p suceeds on unit ~A, unit is added to the sspu-units" unit))
		  
		  ;; -> unit ist sspu -> tue es zu den sspu-units
		  (res2nd=arbitrary-units-2-sspu-units (rest list-of-non-sspu-units) (cons unit list-of-sspu-units)))

	      (progn
		(when r2ntop*trans-debug
		  (omega~message "~%~%r2ntop*trans-debug: sspu-refutable-p fails on unit ~A" unit))
		
		(let* ((next-node-to-decompose (res2nd=get-next-node-to-decompose unit :kind 'arb2sspu)))
		  (if (r2ntop~node-present-p next-node-to-decompose delta-relation)
		      ;; -> Decomponiere diesen Knoten

		      (progn
			(when r2ntop*trans-debug
			  (omega~message "~%~%r2ntop*trans-debug: decomposing node ~A of unit ~A" next-node-to-decompose unit))
			
			(let* ((new-non-sspu-units (if (eq next-node-to-decompose conclusion-node)
						       (decomp~get-conclusion-decomp next-node-to-decompose
										     unit
										     (rest list-of-non-sspu-units)
										     list-of-sspu-units)
						     (decomp~get-premise-decomp next-node-to-decompose
										unit
										(rest list-of-non-sspu-units)
										list-of-sspu-units))))
			  (res2nd=arbitrary-units-2-sspu-units new-non-sspu-units list-of-sspu-units)))
		    ;; Sollte eigentlich nicht passieren|v ! HACK BY AMEIER KANN DOCH PASSIEREN !!
		    (progn
		      ;; (omega~message "In Function res2nd=arbitrary-units-2-sspu-units: Sollte eigentlich nicht passieren!")
		      (setf (r2ntop~decompose-unit-nodes-to-decompose unit)
			    (remove next-node-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit)))
		      (setf (r2ntop~decompose-unit-pre-selected-nodes unit)
			    (remove next-node-to-decompose (r2ntop~decompose-unit-pre-selected-nodes unit)))
		      (res2nd=arbitrary-units-2-sspu-units list-of-non-sspu-units list-of-sspu-units)))))))))
    
    list-of-sspu-units))

#| ------------------------------------------ SSPU-UNIT -> GROUND-SSPU-UNIT + STYLE-THINGS ------------------------------------- |#

(defun res2nd=sspu-units-2-ground-sspu-units (list-of-non-ground-sspu-units list-of-ground-sspu-units)
  
  (when r2ntop*trans-debug
    (omega~message "~%~%~%r2ntop*trans-debug: sspu-units-2-ground-sspu-units, with the following units:")
    (omega~message "~%-----------------------------------------------------------------------------")
    (omega~message "~%The non-ground-sspu-units:")
    (mapcar #'(lambda (unit)
		(omega~message "~%~A" unit))
	    list-of-non-ground-sspu-units)
    (omega~message "~%The ground-sspu-units:")
    (mapcar #'(lambda (unit)
		(omega~message "~%~A" unit))
	    list-of-ground-sspu-units)
    )
    
  (if list-of-non-ground-sspu-units
      (let* ((unit (first list-of-non-ground-sspu-units))
	     (delta-relation (r2ntop~decompose-unit-extdelta-relation unit))
	     (conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	     (integ-check (integ~check-decompose-unit! unit)))
	(if integ-check
	    (progn
	      
	      (when r2ntop*trans-debug
		(omega~message "~%~%r2ntop*trans-debug: integ-check suceeds on unit ~A" unit))
	      
	      ;; -> conclusion erschlagen durch integral-formeln -> unit ist bereits fertig
	      (res2nd=sspu-units-2-ground-sspu-units (rest list-of-non-ground-sspu-units) list-of-ground-sspu-units))
	  
	  (progn
	    
	    (when r2ntop*trans-debug
	      (omega~message "~%~%r2ntop*trans-debug: integ-check fails on unit ~A" unit))
	    
	    
	    (let ((next-node-to-decompose (res2nd=get-next-node-to-decompose unit :kind 'sspu2groundsspu)))
	      (if next-node-to-decompose

		  (if (r2ntop~node-present-p next-node-to-decompose delta-relation)
		      
		      (progn
			(when r2ntop*trans-debug
			  (omega~message "~%~%r2ntop*trans-debug: decomposing node ~A of unit ~A" next-node-to-decompose unit))
			
			(let* ((new-non-ground-sspu-units (if (eq conclusion-node next-node-to-decompose)
							      (decomp~get-conclusion-decomp next-node-to-decompose
											    unit
											    (rest list-of-non-ground-sspu-units)
											    nil)
							    (decomp~get-premise-decomp next-node-to-decompose
										       unit
										       (rest list-of-non-ground-sspu-units)
										       nil))))
			  (res2nd=sspu-units-2-ground-sspu-units new-non-ground-sspu-units list-of-ground-sspu-units)))
		    ;; Sollte eigentlcih nicht passieren|v ! HACK BY AMEIER KANN DOCH PASSIEREN !!
		    (progn
		      ;; (omega~message "In Function res2nd=sspu-units-2-ground-sspu-units: Sollte eigentlich nicht passieren!")
		      (setf (r2ntop~decompose-unit-nodes-to-decompose unit)
			    (remove next-node-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit))
			    )
		      (setf (r2ntop~decompose-unit-pre-selected-nodes unit)
			    (remove next-node-to-decompose (r2ntop~decompose-unit-pre-selected-nodes unit))
			    )
		      
		      (res2nd=sspu-units-2-ground-sspu-units list-of-non-ground-sspu-units list-of-ground-sspu-units)))
		
		(progn
		  
		  (when r2ntop*trans-debug
		    (omega~message "~%~%r2ntop*trans-debug: No further node to decompose in unit ~A, unit is added to the ground-sspu-units" unit))
		  
		  (res2nd=sspu-units-2-ground-sspu-units (rest list-of-non-ground-sspu-units) (cons unit list-of-ground-sspu-units))))))))
    
    list-of-ground-sspu-units))

#| --------------------------------------------- GROUND-SSPU -> PROOF-NODES -------------------------------------------- |#

(defun res2nd=translate-ground-sspu-units (list-of-ground-sspu-units)

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Translating units into assertion applications.")
    (omega~message "~%----------------------------------------------------------------"))
  
  (mapcar #'(lambda (ground-sspu-unit)
	      
	      (when r2ntop*trans-debug
		(omega~message "~%~%r2ntop*trans-debug: Transforming the sspu-unit ~A into sspu-steps." ground-sspu-unit))
	      
	      (let* ((ground-steps (sspu~ground-sspu-steps ground-sspu-unit))) 
		(res2nd=translation-ground-sspu ground-sspu-unit ground-steps)))
	  list-of-ground-sspu-units))


#|
   ACHTUNG: Kann sein, dass es hier noch Probleme mit den Reflex-clauses gibt, da die keine entsprechenden Zeilen und
   delta-pairs haben, wird glaube ich nicht komplett abgefangen

|#

(defun res2nd=translation-ground-sspu (unit sspu-steps)
  (declare (edited  "28-FEB-1996")
	   (authors Ameier)
	   (input  "A decompose unit and and a list of triples <U, List-Unit-Leaves, Non-Unit-Leaf>"
		   "(see res2nd=ground-sspu-steps) as intermediate sspu steps.")
	   (effect "Translates the sspu-resolution into an PDS subproof by translating each triple"
		   "by an assertion application (or special handlings for equality things) and insert"
		   "the new lines into omega*current-proof-plan." )
	   (value  "Undefined"))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Translating unit ~A with sspu-steps ~A" unit sspu-steps))
  
  (let ((degenerated (res2nd=degenerated-sspu-steps-p sspu-steps))
	(conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	(delta-relation (r2ntop~decompose-unit-extdelta-relation unit)) 
	(steps (reverse (cdr sspu-steps)))          ;; cut the empty clause (= last degenerated step)
        (empty-clause (first sspu-steps)))          ;; and reverse the order ( -> reverse order of proof-tree)
    (if (not degenerated)
	
	;; -> nicht degeneriert
	
	(if (not (res2nd=direct-proof-p conclusion-node delta-relation sspu-steps))
	    
	    ;; -> nicht degeneriert + indirekter Beweis noetig 
	    
	    (if (not (res2nd=formula-equal-false-p conclusion-node))
		
		;; -> nicht degeneriert + indirekter Beweis noetig + conclusion node ist nicht gleich 'false'
		
		(let* ((new-nodes (if (logic~negation-p (node~formula conclusion-node))
				      (r2ntop~apply-tactic 'noti (list conclusion-node nil) nil)
				    (r2ntop~apply-tactic 'indirect (list conclusion-node nil) nil)))
		       (delta-pairs (extdelta~relation-pairs delta-relation))
		       (negated-conclusion-node (third new-nodes))
		       (false-node-I (second new-nodes)))
		  
		  ;; replace in delta-relation the conclusion-node by  negated-conclusion-node
		  (mapcar #'(lambda (x)
			      (when (eq (extdelta~pdsnode x) conclusion-node)
				(setf (extdelta~delta-node-of-formula x)  ;; muss hier nicht noch die Position angepasst werden ?
				      negated-conclusion-node)))          ;; naja, hioer wohl nicht mehr noetig HACK AMEIER
			  delta-pairs)
		  
		  (let* ((assertion-nodes (res2nd=insert-assertions! steps delta-relation (list negated-conclusion-node) nil))
			 (empty-clause-parents (second empty-clause))
			 (contra-nodes (mapcar #'extdelta~pdsnode
					       (remove-if-not #'(lambda (x)
								  (member (extdelta~clause x) empty-clause-parents))
							      (extdelta~relation-pairs delta-relation))))
			 (negated-node (if (logic~negation-p (node~formula (first contra-nodes)))
					   (first contra-nodes)
					 (second contra-nodes)))
			 (unegated-node (first (remove negated-node contra-nodes)))			 
			 (new-nodes (r2ntop~apply-tactic 'note (list nil unegated-node negated-node) nil))
			 (false-node (first new-nodes)))
		    (declare (ignore assertion-nodes))
		    (r2ntop~apply-tactic 'weaken (list false-node-I false-node) nil)))
	      
	      ;; -> nicht degeneriert + indirekter Beweis noetig + conclusion node ist gleich 'false'
	      
	      (let* ((assertion-nodes (res2nd=insert-assertions! steps delta-relation nil nil))
		     (empty-clause-parents (second empty-clause))
		     (contra-nodes (mapcar #'extdelta~pdsnode
					   (remove-if-not #'(lambda (x)
							      (member (extdelta~clause x) empty-clause-parents))
							  (extdelta~relation-pairs delta-relation))))
		     (negated-node (if (logic~negation-p (node~formula (first contra-nodes)))
				       (first contra-nodes)
				     (second contra-nodes)))
		     (unegated-node (first (remove negated-node contra-nodes)))			 
		     (new-nodes (r2ntop~apply-tactic 'note (list nil unegated-node negated-node) nil))
		     (false-node (first new-nodes)))
		(declare (ignore assertion-nodes))
		(r2ntop~apply-tactic 'weaken (list conclusion-node false-node) nil)))
	  
	  ;; -> nicht degeneriert + direkter Beweis moeglich
	  
	  (let* ((assertion-nodes (res2nd=insert-assertions! steps delta-relation nil nil)))
	    (r2ntop~apply-tactic 'weaken (list conclusion-node (car (last assertion-nodes))) nil)))
	
	;; -> degeneriert
	
	(let* ((unit-clauses (second (first sspu-steps)))
	       (node1 (res2nd=clause2node (first unit-clauses) delta-relation))
	       (node2 (res2nd=clause2node (second unit-clauses) delta-relation)))
	  (if (res2nd=direct-proof-p conclusion-node delta-relation sspu-steps)
	      (let* ((new-conclusion (extdelta~pdsnode (car (remove-if #'(lambda (x)
									   (eq (extdelta~pdsnode x) conclusion-node))
								       (extdelta~relation-pairs delta-relation)))))) 
		(r2ntop~apply-tactic 'weaken (list conclusion-node new-conclusion) nil))
	    (if (res2nd=formula-equal-false-p conclusion-node)
		(let* ((negated-node (if (logic~negation-p (node~formula node1))
					 node1
				       node2))
		       (unegated-node (first (remove negated-node (list node1 node2)))) 			 
		       (new-nodes (r2ntop~apply-tactic 'note (list nil unegated-node negated-node) nil)))
		  (r2ntop~apply-tactic 'weaken (list conclusion-node (first new-nodes)) nil))
	      (let* ((contra-nodes (mapcar #'extdelta~pdsnode (extdelta~relation-pairs delta-relation)))
		     (negated-node (if (logic~negation-p (node~formula (first contra-nodes)))
				       (first contra-nodes)
				     (second contra-nodes)))
		     (unegated-node (first (remove negated-node contra-nodes)))
		     (new-nodes1 (r2ntop~apply-tactic 'note (list nil unegated-node negated-node) nil))
		     (false-node (first new-nodes1)))
		(r2ntop~apply-tactic 'falsee (list conclusion-node false-node) nil))))))))

(defun res2nd=degenerated-sspu-steps-p (sspu-steps)
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "A list of sspu-steps.")
	   (effect  "None.")
	   (value   "T if the list has length 1, nil otherwise."))
  (if (= (length sspu-steps) 1)
      't
    nil))

#| TESTEN  HACK BY AMEIER|#

(defun res2nd=formula-equal-false-p (node)
  (declare (edited  "13-FEB-1997")
	   (authors Ameier)
	   (input   "A node")
	   (effect  "None.")
	   (value   "T if node's formula is eq to the falsum of the"
		    "current proof plan's environment."))
  (data~equal (node~formula node)          ;; eigentlich eq genug oder ? HACK BY AMEIER
	      (env~lookup-object 'false (pds~environment omega*current-proof-plan))))


(defun res2nd=direct-proof-p (conclusion-node delta-relation sspu-steps)
  (declare (edited  "03-APR-1998")
	   (authors Ameier)
	   (input   "The conclusion node and the list of sspu-steps.")
	   (effect  "None.")
	   (value   "T if there exists only one apperance of a clause, that accords to the"
		    "the conclusion-node and this clause is a unit clause and is only in the"
		    "last step of the sspu-steps. (-> the hole thing can be translated as direct proof)."))
  (let* ((delta-relation-pairs (extdelta~relation-pairs delta-relation))
	 (pairs-of-conc-node (remove-if-not #'(lambda (pair)
						(eq (extdelta~pdsnode pair) conclusion-node))
					    delta-relation-pairs))
	 (clauses-of-conc-node (remove-duplicates (mapcar #'extdelta~clause pairs-of-conc-node)))
	 (sspu-clauses-without-last-step (remove-duplicates
					  (apply 'append (mapcar #'(lambda (step)
								     (let ((unit-clauses (second step))
									   (non-unit-clause (third step)))
								       (cons non-unit-clause unit-clauses)))
								 (rest sspu-steps)))))
	 (sspu-clauses-last-step (second (first sspu-steps))))
    (if (> (length clauses-of-conc-node) 1)
	nil
      (if (member (first clauses-of-conc-node) sspu-clauses-without-last-step)
	  nil
	(if (member (first clauses-of-conc-node) sspu-clauses-last-step)
	    't
	  nil)))))

(defun res2nd=introduce-reflex-nodes-into-delta-relation! (reflex-clause delta-relation)
  (declare (edited  "16-SEP-1996")
	   (authors Ameier)
	   (input   "A refutation graph and a delta-relation.")
	   (effect  "For every reflexivity clause in the refutation graph a corresponding node"
		    "is introduced into the nd-proof, and the delta-relation is updated by"
		    "an according pair.")
	   (value   "The new reflexivity node."))
  (let* ((formula (lit~atom (first (cl~literals reflex-clause))))
	 (new-nodes (r2ntop~apply-tactic '=ref (list nil) (list (first (data~appl-arguments formula))))))
    (extdelta~add-pair! delta-relation
			(r2ntop~new-named-term (node~formula (first new-nodes)))
			(pos~list-position ())
			(first new-nodes)
			reflex-clause
			(pos~list-position (list '0)))
    (first new-nodes)))

#|
(defun res2nd=insert-assertions! (steps delta-relation before-nodes after-nodes)
  (declare (edited  "29-FEB-1996")
	   (authors Ameier)
	   (input   "A list of units <U, List-Leaves, Assertion> (described below),"
		    "the current delta-relation, a list of before-nodes and a list"
		    "of after nodes.")
	   (effect  "The created assertion-nodes are added to the proof and the"
		    "delta-relation is updated with these new nodes and the"
		    "corresponding unit-clauses.")
	   (value   "The newly generated assertion-nodes."))
  (declare (ignore after-nodes before-nodes))
  (do* ((assertion-steps steps (rest assertion-steps))
	(step (car assertion-steps) (car assertion-steps))
	(new-nodes))
      ((null assertion-steps) (reverse new-nodes))
    (alift~initialize omega*current-proof-plan) 
    (let* ((clause (first step))
	   (atom (lit~atom (car (cl~literals clause))))
	   (formula (if (lit~positive-p (car (cl~literals clause)))
			atom                                                  ;; produced-formula by applied-assertion
		      (let ((not (env~lookup-object 'not (pds~environment keim::pds*current-proof-plan))))
			(term~appl-create not (list atom)))))
	   (premises (second step))                                           ;; unit-premises
	   (assertion (third step)))                                          ;; assertion-to-apply

      ;;(format t "~%~%THE STEP: ~A" step)
      ;;(format t "~%~%THE ASSERTION: ~A" assertion)
      ;;(format t "~%~%THE PREMISES: ~A" premises)
      ;;(format t "~%~%THE formula: ~A:" formula)
      
      (cond ((typep assertion 'r2ntop+paramod-clause)
	     (if (keim~equal (second (cl~literals assertion)) (first (cl~literals clause)))      ;; unit-result is equality
		 (let* ((premises-nodes (mapcar #'(lambda (premise)
						    (res2nd=clause2node premise delta-relation))
						premises))
			(negated-premise (if (logic~negation-p (node~formula (first premises-nodes)))
					     (first premises-nodes)
					   (second premises-nodes)))
			(unegated-premise (first (remove negated-premise premises-nodes)))
			(back-nodes (r2ntop~apply-tactic 'neg=i
							 (list nil unegated-premise negated-premise)
							 (list (r2ntop~paramod-clause-paramod-position assertion))))) 
		   (if (keim~equal (node~formula (first back-nodes)) formula)
		       (progn
			 ;; new-node-in-proof
			 (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
					     (pos~add-front 0 (pos~empty))) 
			 ;; update-delta-relation by the new pair of the recursive unit-clause
			 (setq new-nodes (cons (first back-nodes) new-nodes)))
		     (progn
		       (let* ((new-back-nodes (r2ntop~apply-tactic 'neg=sym (list nil (first back-nodes)) nil)))
			 ;; new-node-in-proof
			 (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first new-back-nodes) clause
					     (pos~add-front 0 (pos~empty))) 
			 ;; update-delta-relation by the new pair of the recursive unit-clause
			 (setq new-nodes (cons (first new-back-nodes) new-nodes))))))
	       
	       (let* ((equation-node (if (keim~equal (lit~atom (first (cl~literals (first premises))))
						     (lit~atom (second (cl~literals assertion))))
					 (res2nd=clause2node (first premises) delta-relation)
				       (res2nd=clause2node (second premises) delta-relation)))
		      (term-to-subst-node (if (keim~equal (lit~atom (first (cl~literals (first premises))))
							  (lit~atom (second (cl~literals assertion))))
					      (res2nd=clause2node (second premises) delta-relation)
					    (res2nd=clause2node (first premises) delta-relation)))
		      (mother-position (r2ntop~paramod-clause-paramod-position assertion))
		      (back-nodes (r2ntop~apply-tactic '=subst
						       (list nil term-to-subst-node equation-node)
						       (list (if (logic~negation-p (node~formula term-to-subst-node))
								 (pos~add-front '1 mother-position)
							       mother-position)))))
		 ;; new-node-in-proof
		 
		 (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
				     (pos~add-front 0 (pos~empty))) 
		 ;; update-delta-relation by the new pair of the recursive unit-clause
		 
		 (setq new-nodes (cons (first back-nodes) new-nodes)))))
	    ((typep assertion 'r2ntop+flip-clause)
	     (if (lit~positive-p (first (cl~literals clause))) ;; -> =com-forward
		 (let* ((equation-premise-node (res2nd=clause2node (first premises) delta-relation))
			(back-nodes (r2ntop~apply-tactic '=sym (list nil equation-premise-node) nil)))
		   ;; new-node-in-proof
		   
		   (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
				       (pos~add-front 0 (pos~empty))) 
		   ;; update-delta-relation by the new pair of the recursive unit-clause
		   
		   (setq new-nodes (cons (first back-nodes) new-nodes)))
	       ;; -> neg=sym
	       (let* ((unequation-premise-node (res2nd=clause2node (first premises) delta-relation))
		      (back-nodes (r2ntop~apply-tactic 'neg=sym (list nil unequation-premise-node) nil)))
		 ;; new-node-in-proof
		 
		 (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
				     (pos~add-front 0 (pos~empty))) 
		 ;; update-delta-relation by the new pair of the recursive unit-clause
		 
		 (setq new-nodes (cons (first back-nodes) new-nodes)))))
	    (t
	     (let* ((premises-nodes (mapcar #'(lambda (premise)
						(res2nd=clause2node premise delta-relation))
					    premises))
		    (assertion-node (res2nd=clause2node assertion delta-relation))
		    (new-node (alift~apply-assertion-tactic premises-nodes assertion-node formula))
		    ;; (new-node (alift~apply-command! premises-nodes assertion-node formula)) 
		    ;; (new-appl-reason (pdsn~meth-appl-reason-create (cons assertion-node premises-nodes) nil (list new-node)))
		    )
	       ;; (mapcar #'(lambda (node)
	       ;;	   (pdsn~insert-reason! new-appl-reason node))
	       ;;	       (append (list new-node assertion-node) premises-nodes))
	       
	       ;; ATTENTION: ADDS ASSERTION-node-HYPS TO HYPS -> HACK
	       (setf (pdsn~hyps new-node)
		     (remove-duplicates (append (pdsn~hyps assertion-node) (pdsn~hyps new-node))))
	       ;; new-node-in-proof
	       (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) new-node clause
				   (pos~add-front 0 (pos~empty))) 
	       ;; update-delta-relation by the new pair of the recursive unit-clause
	       (setq new-nodes (cons new-node new-nodes))
	       ))))))
|#

;; New Version with SHORT CUTS:

(defun res2nd=insert-assertions! (steps delta-relation before-nodes after-nodes)
  (declare (edited  "29-FEB-1996")
	   (authors Ameier)
	   (input   "A list of units <U, List-Leaves, Assertion> (described below),"
		    "the current delta-relation, a list of before-nodes and a list"
		    "of after nodes.")
	   (effect  "The created assertion-nodes are added to the proof and the"
		    "delta-relation is updated with these new nodes and the"
		    "corresponding unit-clauses.")
	   (value   "The newly generated assertion-nodes."))
  (declare (ignore after-nodes before-nodes))
  (do* ((assertion-steps steps (rest assertion-steps))
	(all-unit-premise-lines-until-now
	 (apply #'append
		(mapcar #'(lambda (step)
			    (let* ((premises (second step))
				   (premises-nodes (apply #'append
							  (mapcar #'(lambda (premise)
								      (if (null (typep premise 'r2ntop+reflex-clause))
									  (let* ((find-pair-to-clause
										  (find premise (delta~relation-pairs delta-relation)
											:test #'(lambda (cl pair)
												  (eq cl (extdelta~clause pair))))))
									    (if find-pair-to-clause
										;; clause ist beireits in PDS
										(list (extdelta~pdsnode find-pair-to-clause))
									      ;; sonst: premise innerhalb der SSPU Zerlegung
									      nil))
									nil))
								  premises))))
			      premises-nodes))
			steps)))
	(step (car assertion-steps) (car assertion-steps))
	(new-nodes))
      ((null assertion-steps)
       (reverse new-nodes))

    ;; PREVIOPUSLY:
    ;; We created the new PDS lines by calling the assertion level module of Armin.
    ;; This was good for testing (both: The Assertion level module and this transformation stuff)
    ;; but now that the transformation stuff is finished it is only time consuming!
    ;; Hence i decided to do create the assertion level nodesd by hand ...
    ;; (The same REMAKR appears a littlt bit later in the file ...)
    ;; (alift~initialize omega*current-proof-plan) 

    (let* ((clause (first step))
	   (atom (lit~atom (car (cl~literals clause))))
	   (formula (if (lit~positive-p (car (cl~literals clause)))
			atom                                                  ;; produced-formula by applied-assertion
		      (let ((not (env~lookup-object 'not (pds~environment keim::pds*current-proof-plan))))
			(term~appl-create not (list atom)))))
	   (premises (second step))                                           ;; unit-premises
	   (assertion (third step))                                           ;; assertion-to-apply
      
	   (find-result (find formula all-unit-premise-lines-until-now
			      :test #'(lambda (form node)
					(data~equal form (node~formula node))))))
	
      (if find-result
	  ;; SHORT-CUT gefunden -> KEIN neuer EIGENER STEP NOTWENDIG !!!
	  (progn
	    (omega~message "~%SHORT CUT DURING SSPU-TRANSLATION: FORMULA ~A ALREADY FOUND IN LINE ~A" formula find-result)
	    (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) find-result
				clause
				(pos~add-front 0 (pos~empty)))
	    ;; The following is necessary since the new-nodes have to contain always as first step the
	    ;; node corresponding to the last unit-result step!
	    (setq new-nodes (cons find-result new-nodes)))
	
	;; Kein SHORT-CUT -> NEUER STEP ERZEUGEN !!!
	
	;;(format t "~%~%THE STEP: ~A" step)
	;;(format t "~%~%THE ASSERTION: ~A" assertion)
	;;(format t "~%~%THE PREMISES: ~A" premises)
	;;(format t "~%~%THE formula: ~A:" formula)
	
	(progn
	  
	  (cond ((typep assertion 'r2ntop+paramod-clause)
		 (if (keim~equal (second (cl~literals assertion)) (first (cl~literals clause)))      ;; unit-result is equality
		     (let* ((premises-nodes (mapcar #'(lambda (premise)
							(res2nd=clause2node premise delta-relation))
						    premises))
			    (negated-premise (if (logic~negation-p (node~formula (first premises-nodes)))
						 (first premises-nodes)
					       (second premises-nodes)))
			    (unegated-premise (first (remove negated-premise premises-nodes)))
			    (back-nodes (r2ntop~apply-tactic 'neg=i
							     (list nil unegated-premise negated-premise)
							     (list (r2ntop~paramod-clause-paramod-position assertion))))) 
		       (if (keim~equal (node~formula (first back-nodes)) formula)
			   (progn
			   ;; new-node-in-proof
			     (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
					       (pos~add-front 0 (pos~empty))) 
			     ;; update-delta-relation by the new pair of the recursive unit-clause
			     (setq new-nodes (cons (first back-nodes) new-nodes)))
			 (progn
			   (let* ((new-back-nodes (r2ntop~apply-tactic 'neg=sym (list nil (first back-nodes)) nil)))
			     ;; new-node-in-proof
			     (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first new-back-nodes) clause
						 (pos~add-front 0 (pos~empty))) 
			     ;; update-delta-relation by the new pair of the recursive unit-clause
			     (setq new-nodes (cons (first new-back-nodes) new-nodes))))))
		   
		   (let* ((equation-node (if (keim~equal (lit~atom (first (cl~literals (first premises))))
							 (lit~atom (second (cl~literals assertion))))
					     (res2nd=clause2node (first premises) delta-relation)
					   (res2nd=clause2node (second premises) delta-relation)))
			  (term-to-subst-node (if (keim~equal (lit~atom (first (cl~literals (first premises))))
							      (lit~atom (second (cl~literals assertion))))
						  (res2nd=clause2node (second premises) delta-relation)
						(res2nd=clause2node (first premises) delta-relation)))
			  (mother-position (r2ntop~paramod-clause-paramod-position assertion))
			  (back-nodes (r2ntop~apply-tactic '=subst
							   (list nil term-to-subst-node equation-node)
							   (list (if (logic~negation-p (node~formula term-to-subst-node))
								     (pos~add-front '1 mother-position)
								   mother-position)))))
		     ;; new-node-in-proof
		     (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
					 (pos~add-front 0 (pos~empty))) 
		     ;; update-delta-relation by the new pair of the recursive unit-clause
		     (setq new-nodes (cons (first back-nodes) new-nodes)))))
		((typep assertion 'r2ntop+flip-clause)
		 (if (lit~positive-p (first (cl~literals clause))) ;; -> =com-forward
		     (let* ((equation-premise-node (res2nd=clause2node (first premises) delta-relation))
			    (back-nodes (r2ntop~apply-tactic '=sym (list nil equation-premise-node) nil)))
		       ;; new-node-in-proof
		       
		       (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
					   (pos~add-front 0 (pos~empty))) 
		       ;; update-delta-relation by the new pair of the recursive unit-clause
		       
		       (setq new-nodes (cons (first back-nodes) new-nodes)))
		   ;; -> neg=sym
		   (let* ((unequation-premise-node (res2nd=clause2node (first premises) delta-relation))
			  (back-nodes (r2ntop~apply-tactic 'neg=sym (list nil unequation-premise-node) nil)))
		     ;; new-node-in-proof
		     (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) (first back-nodes) clause
					 (pos~add-front 0 (pos~empty))) 
		     ;; update-delta-relation by the new pair of the recursive unit-clause
		     (setq new-nodes (cons (first back-nodes) new-nodes)))))
		(t
		 (let* ((premises-nodes (mapcar #'(lambda (premise)
						    (res2nd=clause2node premise delta-relation))
						premises))
			(assertion-node (res2nd=clause2node assertion delta-relation))
			;; PREVIOPUSLY:
			;; We created the new PDS lines by calling the assertion level module of Armin.
			;; This was good for testing (both: The Assertion level module and this transformation stuff)
			;; but now that the transformation stuff is finished it is only time consuming!
			;; Hence i decided to do create the assertion level nodesd by hand ...
			;; (The same REMAKR appears a littlt bit earlier in the file ...)
			;; (new-node (alift~apply-assertion-tactic premises-nodes assertion-node formula))

			(ass-just (alj~create (cons assertion-node premises-nodes)))
			(new-node (pdsn~create (pds~new-node-name omega*current-proof-plan)
					       (remove-duplicates
						(apply #'append (mapcar #'(lambda (node)
									    (pdsn~hyps node))
									(cons assertion-node premises-nodes))))
					       formula
					       ass-just))
			;; OLD STUFF:	
			;; (new-node (alift~apply-command! premises-nodes assertion-node formula)) 
			;; (new-appl-reason (pdsn~meth-appl-reason-create (cons assertion-node premises-nodes) nil (list new-node)))
			)

		   ;; NEW STUFF:
		   (pds~only-insert-node! new-node)
		   
		   ;; OLD STUFF:
		   ;; (mapcar #'(lambda (node)
		   ;;	   (pdsn~insert-reason! new-appl-reason node))
		   ;;	       (append (list new-node assertion-node) premises-nodes))
		   
		   ;; ATTENTION: ADDS ASSERTION-node-HYPS TO HYPS -> HACK
		   (setf (pdsn~hyps new-node)
			 (remove-duplicates (append (pdsn~hyps assertion-node) (pdsn~hyps new-node))))
		   ;; new-node-in-proof
		   (extdelta~add-pair! delta-relation (r2ntop~new-named-term formula) (pos~empty) new-node clause
				       (pos~add-front 0 (pos~empty))) 
		   ;; update-delta-relation by the new pair of the recursive unit-clause
		   (setq new-nodes (cons new-node new-nodes))
		   )))

	  ;; immer wenn neuer Schritt gemacht wird entsteht neuer Knoten
	  ;; dieser steht in new-nodes an Schritt eins!
	  ;; Jede neue Zeile ist nun auch eine zum SHORT-CUT benutzbare Uni-clause
	  ;; -> einfuegen in all-unit-premise-lines-until-now
  
	  (setq all-unit-premise-lines-until-now (append all-unit-premise-lines-until-now (list (first new-nodes))))
	  )))))

;; in this do* loop the asserion-nodes are computed by from the units
;; and the delta-realtion is updated by the new-produced-nd-nodes
;; and the according unit-clauses
;; A basic working assumption: the instantiation of a sspu-reslution proof does not involve splitting,
;; and therefore preserves the structure of the sspu-resolution.
;; The current algorithm always instantiates the premises to ground atoms, which needs further refinement


#| ------------------------------------------------ Choosing the next node to decompose -------------------------------- |#


;; CASE1: There exists a node to decompose:
;; to receive sspu-resolution prefer and-conclusion or or/imp-premise-nodes, cause with this nodes you get
;; a splitting and with this possibly sspu.
;; Case2: There exists no node to decompose:
;; compute for all pdsnodes in delta-relation the length of the biggest clause, who accord to them.
;; Then seek the pdsnode who has the smallest 'biggest clause'.
;; This pdsnode has to be decomposed, till there is a splitting, that means don't check the result of decomposition
;; of this node about necessarity (unit-premise/rec-existential) ,but decompose till there's a splitting.
;; (for example (forall [x] (or (s x) (q x))) -> decomposition -> (or (s a) (q a)) doesn't changes anything in
;; the sspu-situation -> don't rethink about if this node has to be decomposed, but make further decomposition
;; this is done with the global-variable r2ntop*check-flag ,default it is t but now it is set to nil
;; and reset to t only by a splitting.

(defun res2nd=get-next-node-to-decompose (unit &key (kind nil))

  (when r2ntop*trans-debug
    (omega~message "~%~%r2ntop*trans-debug: Choosing next node to decompose in unit ~A" unit))
  
  (let* ((pre-selected-nodes (r2ntop~decompose-unit-pre-selected-nodes unit))
	 (conclusion-node (r2ntop~decompose-unit-conclusion-node unit)))
    (if pre-selected-nodes
	
	(progn
	  (when r2ntop*trans-debug
	    (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first of the preselected-nodes."
			   (first pre-selected-nodes)))
	  (first pre-selected-nodes))
      
      (let* ((next-node-to-decompose (res2nd=choose-node-to-decompose unit :kind kind)))
	(if next-node-to-decompose
	    (if r2ntop*avoid-doubeling

		(progn
		  (when r2ntop*trans-debug
		    (omega~message "~%~%r2ntop*trans-debug: Checking for new preselected nodes, to avoid duplication."))
		  
		  (let ((formula (node~formula next-node-to-decompose)))
		    (if (or (and (not (eq next-node-to-decompose conclusion-node)) 
				 (or (logic~disjunction-p formula) (logic~implication-p formula)))
			    (and (eq next-node-to-decompose conclusion-node) (logic~conjunction-p formula)))
			(progn
			  (lord~check-unit-before-splitting! unit next-node-to-decompose)
			  (let* ((new-pre-selected-nodes (r2ntop~decompose-unit-pre-selected-nodes unit)))
			    (if new-pre-selected-nodes

				(progn
				  (when r2ntop*trans-debug
				    (omega~message "~%~%r2ntop*trans-debug: New preselected nodes: ~A, choosing node ~A instead of node ~A"
						   new-pre-selected-nodes
						   (first new-pre-selected-nodes)
						   next-node-to-decompose))
				  (first new-pre-selected-nodes))

			      (progn
				(when r2ntop*trans-debug
				  (omega~message "~%~%r2ntop*trans-debug: No new preselected nodes, keeping node ~A"
						 next-node-to-decompose))
				next-node-to-decompose))))
		      
		      (progn
			(when r2ntop*trans-debug
			  (omega~message "~%~%r2ntop*trans-debug: No new preselected nodes, keeping node ~A"
					 next-node-to-decompose))
			next-node-to-decompose))))
	      next-node-to-decompose)
	  nil)))))

(defun res2nd=choose-node-to-decompose (unit &key (kind nil))
  (let* ((nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit))
	 (conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation unit)))
    (if nodes-to-decompose
	(let* ((and/or/imp-split-nodes (remove-if-not #'(lambda (x)  
							  (let ((formula (node~formula x)))
							    (or (and (eq x conclusion-node) 
								     (logic~conjunction-p formula))
								(and (not (eq x conclusion-node)) 
								     (logic~disjunction-p formula))
								(and (not (eq x conclusion-node)) 
								     (logic~implication-p formula)))))
						      nodes-to-decompose))
	       (other-nodes-to-decompose (r2ntop~remove-list and/or/imp-split-nodes nodes-to-decompose))
	       (forall-nodes (remove-if-not #'(lambda (x)
						(or
						 (and 
						  (logic~universal-quantification-p (node~formula x))
						  (not (eq x conclusion-node)))
						 (and
						  (logic~existential-quantification-p (node~formula x))
						  (eq x conclusion-node))))
					    other-nodes-to-decompose))
	       (possible-forall-nodes (remove-if-not #'(lambda (forall-node)
							 (res2nd=forall-node-can-be-decomposed-p
							  forall-node delta-relation))
						     forall-nodes))
	       (not-forall-other-nodes (r2ntop~remove-list forall-nodes other-nodes-to-decompose)))
	  (cond (possible-forall-nodes

		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first possible forall-node in the nodes-to-decompose."
				  (first possible-forall-nodes)))
		 
		 (first possible-forall-nodes))
		(not-forall-other-nodes
		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first non-splitting,non-forall node in the nodes-to-decompose (possible forall nodes was empty)."
				  (first not-forall-other-nodes)))
		 (first not-forall-other-nodes))
		(t
		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first first possible node at all in the nodes to decompose (possible-forall nodes and non-splitting-nodes was both empty)." 
				  (first and/or/imp-split-nodes)))
		 (first and/or/imp-split-nodes))))
      (if (equal kind 'arb2sspu)
	  (if (eq r2ntop*reach-sspu-style 'case)
	      (adv~smaller-sspu! unit)
	    (adv~get-smallest-node-to-split! unit))
	(adv~make-further-decompose-in-sspu! unit)))))

#|

Diese VARIANTE PRAEFERIERT die SPLITTING KNOTEN !
NUR ZU TESTZWECKEN ZU GEBRAUCHEN!!!!


(defun res2nd=choose-node-to-decompose (unit &key (kind nil))
  (let* ((nodes-to-decompose (r2ntop~decompose-unit-nodes-to-decompose unit))
	 (conclusion-node (r2ntop~decompose-unit-conclusion-node unit))
	 (delta-relation (r2ntop~decompose-unit-extdelta-relation unit)))
    (if nodes-to-decompose
	(let* ((and/or/imp-split-nodes (remove-if-not #'(lambda (x)  
							  (let ((formula (node~formula x)))
							    (or (and (eq x conclusion-node) 
								     (logic~conjunction-p formula))
								(and (not (eq x conclusion-node)) 
								     (logic~disjunction-p formula))
								(and (not (eq x conclusion-node)) 
								     (logic~implication-p formula)))))
						      nodes-to-decompose))
	       (other-nodes-to-decompose (r2ntop~remove-list and/or/imp-split-nodes nodes-to-decompose))
	       (forall-nodes (remove-if-not #'(lambda (x)
						(or
						 (and 
						  (logic~universal-quantification-p (node~formula x))
						  (not (eq x conclusion-node)))
						 (and
						  (logic~existential-quantification-p (node~formula x))
						  (eq x conclusion-node))))
					    other-nodes-to-decompose))
	       (possible-forall-nodes (remove-if-not #'(lambda (forall-node)
							 (res2nd=forall-node-can-be-decomposed-p
							  forall-node delta-relation))
						     forall-nodes))
	       (not-forall-other-nodes (r2ntop~remove-list forall-nodes other-nodes-to-decompose)))
	  (cond (and/or/imp-split-nodes

		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first SPLITTING-NODE !!!!!!!!!"
				  (first and/or/imp-split-nodes)))

		 (first and/or/imp-split-nodes))
		
		(possible-forall-nodes
		 
		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first possible forall-node in the nodes-to-decompose."
				  (first possible-forall-nodes)))
		 
		 (first possible-forall-nodes))
		(not-forall-other-nodes
		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first non-splitting,non-forall node in the nodes-to-decompose (possible forall nodes was empty)."
				  (first not-forall-other-nodes)))
		 (first not-forall-other-nodes))
		(t

		 (when r2ntop*trans-debug
		   (omega~message "~%~%r2ntop*trans-debug: Choosing node ~A, because it's the first possible forall-node in the nodes-to-decompose."
				  (first possible-forall-nodes)))
		 
		 (first possible-forall-nodes))))
      (if (equal kind 'arb2sspu)
	  (if (eq r2ntop*reach-sspu-style 'case)
	      (adv~smaller-sspu! unit)
	    (adv~get-smallest-node-to-split! unit))
	(adv~make-further-decompose-in-sspu! unit)))))

|#


#| ----------------------------------------------------------- COPY REFLEX-CLAUSES ---------------------------------------------- |#

(defvar res2nd*type-var-counter 0)

(defun res2nd=copy-reflex-clauses! (res-proof)
  (declare (edited  "14-AUG-2000")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "For each occurence of the/a reflex-clause in a step of the resolution proof"
		    "a new reflex-clause is created that replaces the old one in the justification"
		    "of the step. Furthermore, these new reflex-clauses are added to the steps of"
		    "the proof.")
	   (value   "The resolution proof."))
  (let* ((steps (res~proof-step-clauses res-proof)))
    (mapcar #'(lambda (step-clause)
		(res2nd=check-for-reflex-clause! step-clause res-proof))
	    steps)

    res-proof))

(defun res2nd=check-for-reflex-clause! (clause res-proof)
  (declare (edited  "14-AUG-2000")
	   (authors Ameier)
	   (input   "A step clause (that is a non-initial clause) of a resolutiomn proof and the"
		    "resolution proof itself.")
	   (effect  "If the step-clause contains in its parents a reflex-clause and this reflex-clause"
		    "is properly used in the step (as resolution partner or as father-clause, but not"
		    "as mother clause, and the variables of the reflex-clause are used) then it is replaced"
		    "by a new reflex-clause which is added to the steps of the resolution proof.")
	   (value   "UNdefined."))
  
  (let* ((just (node~justification clause))
	 (parents (res~justification-parents just))
	 (mgu (res~justification-unifier just))
	 (new-parents (do* ((rest-parents parents (rest rest-parents))
			    (i 0 (+ i 1))
			    (back-parents nil))
			  ((null rest-parents)
			   back-parents)
			(let* ((head (first rest-parents)))
			  (if (res2nd=completly-used-reflex-p head mgu just)
			      (let* ((new-reflex-clause (res2nd=create-and-insert-new-reflex-clause! head mgu res-proof)))
				(setf back-parents (append back-parents (list new-reflex-clause))))
			    (setf back-parents (append back-parents (list head))))))))
    (setf (res~justification-parents just) new-parents)))

(defun res2nd=create-and-insert-new-reflex-clause! (old-reflex-clause subst res-proof)
  (declare (edited  "14-AUG-2000")
	   (authors Ameier)
	   (input   "A reflex-clause, a substitition, and a resolution proof.")
	   (effect  "A new reflex-clause is created, it is inserted into the resolution proof, and its variable"
		    "and type-variable replace the variable and type-variable of the old reflex-clause in the"
		    "substitition.")
	   (value   "The new reflex-clause."))
  (let* ((env (res~proof-environment res-proof))
	 (new-type-var (type~variable-create (intern (format nil "~A~A" 'bb (incf res2nd*type-var-counter))
						     (find-package :omega))))
	 (new-var (term~generate-term-primitive-with-new-name 'x- new-type-var 'term+variable env))
	 (new-term (term~appl-create (env~lookup-object '= env) (list new-var new-var)))
	 (new-clause (cl~create (list (lit~literal-create new-term
							  't))
				:justification (res~reflex-create (gensym))))
	 (old-var (first (data~appl-arguments (lit~atom (first (cl~literals old-reflex-clause))))))
	 (old-type-var (term~type old-var)))
    
    ;;(format t "~%~%THE OLD-SUBST: ~A" subst)
    
    ;; add new clause to the steps of the resolution proof
    (setf (res~proof-clauses res-proof)
	  (append (res~proof-clauses res-proof) (list new-clause)))
    ;; replace the old-type-var and old-var by the new things in the substitution
    (setf (subst~domain subst)
	  (res2nd=replace new-type-var old-type-var
			    (res2nd=replace new-var old-var (subst~domain subst))))
    
    ;;(format t "~%~%THE NEW-SUBST: ~A" subst)
    
    new-clause))


(defun res2nd=replace (new old list &key (test #'eq))
  (mapcar #'(lambda (item)
	      (if (funcall test item old)
		  new
		item))
	  list))

(defun res2nd=completly-used-reflex-p (clause subst just)
  (declare (edited  "14-AUG-2000")
	   (authors Ameier)
	   (input   "A clause, a substitution, and a justification.")
	   (effect  "None.")
	   (value   "T if the clause is a reflex-clause, the justification is a resolution or paramodulation,"
		    "and the substitition mapps the variable and the type-variable of the reflex-clause."))
  (if (and (res~reflex-p (node~justification clause))
	   (or (res~hyper-resolution-p just)
	       (res~ur-resolution-p just)
	       (res~resolution-p just)
	       (res~paramodulation-p just)))
      ;; clause is reflex-clause and is used in a resolution or paramodulation
      (let* ((literal (first (cl~literals clause)))
	     (var (first (data~appl-arguments (lit~atom literal))))
	     (type-var (term~type var))
	     (dom (subst~domain subst)))
	(if (and (find var dom)
		 (find type-var dom))
	    't
	  nil))
    nil))
