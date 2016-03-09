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


(mod~defmod LEMMA 
            :uses (cl data delta env extdelta gsub keim lit logic node omega pds pdsn pos prob r2ntop res sksym subst term termix)
            :documentation "Splitting Resolution proofs to introduce lemmas and all therefor needed Operations."
            :exports (
                      
                      lemma~pull-the-exists!
                      lemma~split-res-proof!))


;;; The following functions are internal in other modules and should not be used:
;;; (hocnf=normalize-named-formula)


;; NOCH NICHT DRIN: HEURISTICS !!!
;;                  Wann Lemmatisierung ?

;; Es gibt zwei Faelle, wann die Anwendung eines Lemmas interessant ist, diese Faelle ergeben sich aus der Herstellung der
;; Ground-substitution:
;; 1. Eine Unit-Klause mit mehreren Ground-References -> fuehrt zu Verdoppelung            -> Vermeiden !!
;; 2. Eine non-unit Klause liegt auf mehreren Aesten -> selbst bei gleichen Ground-references Verdoppelung -> Vermeiden !!
;;
;; -> zerbeche den Resolution proof an entsprechenden Stellen und erzeuge Unterbeweise zu lemmata
;; Leider ist es so, dass das drueberlegen der Grundsubstitution uber einen Resolutionsbeweis selbst schon ein sehr aufwendiger
;; Prozess sein kann (man nehme z.B. GRP186-1 von den Waldmeister beispielen u.v.a.).
;; Deshalb wurde approximierend zur oberen Vorgabe folgendes Implementiert (siehe lemma=lemma-clause-p)
;; Man beachte: Mehr als eine ground-referenz -> mehr als ein children !!
;; Der umkehr Schluss gilt zwar nicht, trotzdem approximieren wir die Anzahl der ground-referenzes mittels
;; der Anzahl der Children, da das Berechnen der ground-referenzes sehr (SEHR !) teuer sein kann, und wir
;; mittels der Lemmatisierung gerade solche Komplexitateten zerschlagen wollen !

;; Warueber Lemmatisiert wird haengt ab von der Variable  r2ntop*lemma, die durch die Eingabe spezifiziert wird (siehe ref2nd-main).
;; Es gibt drei Moeglichkeiten:
;; falls nil       -> keine Lemmatisiserung
;; falls free      -> Lemmatisierung nur ueber Lemmas die keinerlei Skolem Sachen enthalten
;; falls constants -> Lemmatisierung nur ueber Lemmas mit Skolem-Konstanten, nicht uber Lemmas mit Skolem-Funktionen
;; sonst           -> volle Lemmatisierung
;; Der springende Punkt zwischen constants und sonst ist, dass zur Lemmatisierung ueber skolem-functionen die entsprechenden
;; exists quantificationen nach vorne gezogen werden (wie sonst auch) nur dass sie dabei uber forall quantifizierte
;; Sachen drueber gezogen werden muessen, wobei eine Funktions-variable im Quantor entsteht
;; (forall x exists y p(x,y) -> exists f_y forall x p(x,f_y(x))). Dies gilt nur, falls man das Axiom of Choice voraussetzt !!
;; Da dies nicht jedermanns Sache ist wird das halt hier als Regelbar eingestellt.

;; Es ist moeglich, dass Lemma-clauses von clauses der conclusion node abhaengen. In diesem Fall muesste man den Beweis indirect
;; fuehren um dieses Lemma wirklich einfuehren zu koennen. Dies ist erlaubt in folgenden Faellen (siehe Funktion
;; lemma=indirect-proof-is-allowed-p)
;; 1. Der Beweis ist sowieso per Eingabe auf indirekt gesetzt (r2ntop*indirect-proof ist t)
;; 2. Der Style steht auf compact oder auto (r2ntop*sspu-style auf aut oder sspu)
;; SPAETER REVIDIERT ZU: 2. Der Style steht auf compact (AMEIER 16.11.99)
;; NOCH SPAETER REVIDIERT ZU: 2. Der Style steht auf compact oder auto (r2ntop*sspu-style auf aut oder sspu) (es is besser ...)

;; Eine clause wird dann als Lemma-clause in betracht gezogen wenn wie oben geschildert zu befuerchten ist, dass eine
;; Verdoppelung von ihr ausgeht UND, um eine Lemma-Schwemma zu verhindern und wirklich kuerzere Beweise zu bekommen, wenn man davon
;; ausgehen kann, dass man tatsaechlich durch die Einfuehrung des Lemmas ein paar Schritte spart.
;; Dazu wurde folgende Heuritic implementiert (siehe lemma=lemma-clause-p und lemma=self-tree-above-clause-is-big-enough-p):
;; Es wird versucht abzuschaetzen wie hoch die gesparten Schritte im Verhaeltnis zu den zu machenden Schritten sind,um das Lemma
;; einzufuehren.
;; Kosten beim Einfuehren: Pro literal ein OR-mehr, pro Variable ein forall mehr
;;                         -> Anzahl Literals + Anzahl Variablen
;; gepsparte Schritte: Schritte im Baum uber der Lemma-Klause bis die naechsten potentiell als Lemma-clauses in Frage kommenden parents
;;                     erreicht sind * die Anzahl der Kinder der clause
;; Alle diese Werte sind natuerlich nur aproximativ !!
;; Ergibt sich ein Wert von gepsparte Schritte / Kosten beim Einfuehren > r2ntop*lemma-relative-weight (momentan = 1)
;; so sollte das Lemma eingefuert werden.
;; Man kann daruber nachdenken, ob es Sinnvoll sein koennte r2ntop*lemma-relative-weight ebenfalls per Eingabe vorgeben zu lassen
;; um auch mit diesem Wert herumzuspielen.
;;
;; Fuer jedes Lemma das eingefuehrt wird, das Skolem-Sachen enthaelt (ob Skolem-Funktion oder Skolem-Constant ist egal) muessen, die
;; entsprechenden exists, von denen die Skolem-Sachen stammen zuerst eliminiert werden und durch Constanten bzw. Funktionen ersetzt
;; werden.
;; Z.B. Angenommen wir haben eine Lemma Klause [+P(sk)] fuer die wir das Lemma P(c1) herstellen wollen. Dann muessen in allen
;; Decompose-units die P(sk) durch P(c1) ersetzt werden. Dies kann NICHT WAEHREND DES BEWEISENS PASSIEREN, da wir nicht garantieren
;; koennen, dass dann alle exists auf einmal ersetzt werden.
;; Nehmen wir mal an, sk stammt von einem Knoten (or A (exists x R(x))), dann ist das Problem, dass dieser OR-Knoten vielleicht in
;; verschiedenen Decompose-Units verschieden behandelt wuerde! Ausserdem muss aufgrund der Regel ORE dieser Knoten dann fuer jede
;; Decompose Unit getrennt zerlegt werden (wir koennen kein ORE ueber mehrere Units, d.h. ueber mehrere Conclusion nodes machen).
;; Also bekaemen wir zweimal (exists x R(x)) und muessten dann zweimal existse machen, woraus wir zwei Konstanten c1 und c1' bekaemen.
;; Das laesst sich dann aber nicht mehr zu einem Lemma mergen ...
;; Deshalb: Pullen der exists nach vorne (auch wenns nur um Konstanten geht) und dann gleich zu begin schon existse machen und die
;;          entsprechenden Skolem-Sachen ersetzen.
;;
;; Man koennte diese Sache vielleicht noch etwas aufweichen, fuer den Fall, dass wir garantieren koennen, dass eine exists Formel nicht
;; zweimal aufgeloest werden muss, z.B. in dem Fall (and A (exists x. R(x))). Da ein ande immer ueber alle Units gemnacht wird, sollte
;; das exists x.R(x) immer dasselbe sein fuer alle Units.
;;
;; Schau mer mal ob ich sowas noch einbaue ...
;;
;;


#| -------------------------------------------------------- LEMMA SPLIT RES PROOF --------------------------------------------------- |#

(defun lemma~split-res-proof! (res-proof extdelta-relation conclusion-node)
  (declare (edited  "27-OCT-1998")
	   (authors Ameier)
	   (input   "A resolution proof, an extdelta-relation, a conclusion node and a list of"
		    "other triples of this things.")
	   (effect  "Splits theresolution proof, by detecting lemmas in it. This can also cause"
		    "a change in the other resoluion proofs in the other triples."
		    "The found lemma-nodes are inserted into the omega*current-proof-plan."
		    "New pulled-lines are created (see function lemma=pull-the-exists!)")
	   (value   "A list of triples of conclusion-node, res-proof and extdelta-relation."))
  
  (omega~message "~%~% Searching for lemmata ...")
  
  (let* ((empty-clause (res~proof-empty-clause res-proof))
	 (clauses (res~proof-clauses res-proof)))

    ;; stelle sicher, dass keine entsprechenden plist Eintrage bereits vorhanden sind:
    (mapcar #'(lambda (clause)
		(keim~remprop clause 'children)
		(keim~remprop clause 'ancestors))
	    (res~proof-clauses res-proof))
    
    ;; laufe ueber Baum und verkette Clauseln doppelt: in die plist noch einen Eintrag Children mit Liste von Klauseln
    ;; die eine Children sind.
    (mapcar #'(lambda (step-clause)
		(let* ((parents (res~justification-parents (node~justification step-clause))))
		  (mapcar #'(lambda (parent)
			      (keim~put parent 'children (cons step-clause (keim~get parent 'children))))
			  parents)))
	    (res~proof-step-clauses res-proof))

    ;; in jede step-clause einen Eintrag ancestors
    (lemma=set-ancestors! empty-clause (node~justification empty-clause))
        
    ;; man beachte: Die Lemma-Klauses sind genau in der Ordnung wie man sie braucht !!
    ;; Kein DESCEDANT vor seinem ANCESTOR !!
    (let* ((lemma-clauses (lemma=get-lemma-clauses (res~proof-step-clauses res-proof))))

      ;; (omega~message "~%~% The following clauses are choosen to create lemmata:")
      ;; (mapcar #'(lambda (clause)
      ;;  	       (omega~message "~% ~A" clause))
      ;;	    lemma-clauses)
            
      (when r2ntop*trans-debug
	(omega~message "~%~%r2ntop*trans-debug: Found the following possible lemma-clauses: ~A" lemma-clauses))
      
      (multiple-value-bind
	  (new-res-proofs new-extdelta-relations new-conclusion-nodes)
	  (lemma=insert-lemmata! lemma-clauses res-proof extdelta-relation)
	
	;; entferne eintraege children, ancestors und ground-referenzes aus den plist der clauses
	(mapcar #'(lambda (res-proof)
		    (mapcar #'(lambda (clause)
				(keim~remprop clause 'children)
				(keim~remprop clause 'ground-referenzes)
				(keim~remprop clause 'ancestors))
			    (res~proof-clauses res-proof)))
		(cons res-proof new-res-proofs))

	(setq r2ntop*lemma-skolems (remove-duplicates r2ntop*lemma-skolems))
	
	(mapcar #'(lambda (res-proof extdelta-relation conclusion-node)
		    (list res-proof extdelta-relation conclusion-node))
		;; in den Resolutions beweisen muessen die Klauseln kopiert werden, da es sonst bei destructiven Operation
		;; (z.B. Filter flips) zu PRoblemen kommen kann
		(cons res-proof (mapcar #'lemma=copy-res-proof-steps! new-res-proofs))
		(cons extdelta-relation new-extdelta-relations)
		(cons conclusion-node new-conclusion-nodes))))))


#| ----------------------------- Zerbroeseln des res-proof, Einsetzen von Lemmata und Updaten ----------------------------------------- |#


(defun lemma=insert-lemmata! (lemma-clauses res-proof extdelta-relation)

  (let* ((conclusion (res~proof-conclusion res-proof))
	 (conclusion-node (pds~label2node (keim~name conclusion))))
    
    (do* ((rest-lemma-clauses lemma-clauses (rest rest-lemma-clauses))
	  (new-res-proofs nil)
	  (new-extdelta-relations nil)
	  (new-conclusion-nodes nil))
	((null rest-lemma-clauses)
	 (values new-res-proofs
		 new-extdelta-relations
		 new-conclusion-nodes
		 ))
      
      (let* ((lemma-clause (first rest-lemma-clauses))
	     (ancestors (lemma=get-ancestors lemma-clause))
	     (initial-ancestors (remove-if-not #'(lambda (ancestor)
						   (res~initial-p (node~justification ancestor)))
					       ancestors))
	     (extdelta-pairs-of-initial-ancestors (remove-if-not #'(lambda (pair)
								     (find (extdelta~clause pair) initial-ancestors))
								 (delta~relation-pairs extdelta-relation)))
	     (nodes-of-pairs-of-initial-ancestors (remove-duplicates
						   (mapcar #'extdelta~pdsnode
							   extdelta-pairs-of-initial-ancestors))))
	
	(when (or (null (find conclusion-node nodes-of-pairs-of-initial-ancestors))
		  (lemma=indirect-proof-is-allowed-p))

	  (when (find conclusion-node nodes-of-pairs-of-initial-ancestors)
	    (setf r2ntop*indirect-proof-forced 't))
	  
	  (multiple-value-bind
	      (new-res-proof new-extdelta-relation new-conclusion-node)
	      (lemma=insert-lemma! lemma-clause res-proof extdelta-relation)
	    
	    (setq new-res-proofs (append new-res-proofs (list new-res-proof)))
	    (setq new-extdelta-relations (append new-extdelta-relations (list new-extdelta-relation)))
	    (setq new-conclusion-nodes (append new-conclusion-nodes (list new-conclusion-node)))))))))


(defun lemma=indirect-proof-is-allowed-p ()
  (declare (edited  "14-JAN-1999")
	   (authors Ameier)
	   (input   "Nothing.")
	   (effect  "None.")
	   (value   "Computes from the global flag settings, whether it is allowed to use also lemma"
		    "which depend from the conclusion node. This will cause an indirect proof."))


  ;; Es ist moeglich, dass Lemma-clauses von clauses der conclusion node abhaengen. In diesem Fall muesste man den Beweis indirect
  ;; fuehren um dieses Lemma wirklich einfuehren zu koennen. Dies ist erlaubt in folgenden Faellen (siehe Funktion
  ;; lemma=indirect-proof-is-allowed-p)
  ;; 1. Der Beweis ist sowieso per Eingabe auf indirekt gesetzt (r2ntop*indirect-proof ist t)
  ;; 2. Der Style steht auf compact oder auto (r2ntop*sspu-style auf aut oder sspu)
  ;; SPAETER REVIDIERT ZUL: 2. Der Style steht auf compact (AMEIER 16.11.99)
  ;; NOCH SPAETER REVIDIERT ZU: 2. Der Style steht auf compact oder auto (r2ntop*sspu-style auf aut oder sspu) (es is besser ...)
  
  (cond (r2ntop*indirect-proof
	 't)
	((or (equal r2ntop*sspu-style 'aut)
	     (equal r2ntop*sspu-style 'sspu))
	 ;; (equal r2ntop*sspu-style 'sspu)
	 't)
	(t
	 nil)))

(defun lemma=insert-lemma! (lemma-clause res-proof extdelta-relation)

  (multiple-value-bind
      (new-res-proof new-extdelta-relation lemma-node named-lemma-formula)
      (lemma=compute-new-res-proof-and-extdelta lemma-clause res-proof extdelta-relation)
    
    (when r2ntop*trans-debug
      (omega~message "~%~%r2ntop*trans-debug: Insert the lemma-clause ~A, producing lemma-node ~A, with formula ~A"
		     lemma-clause
		     lemma-node
		     (node~formula lemma-node)))

    (setq r2ntop*lemma-nodes (cons lemma-node r2ntop*lemma-nodes))
    
    (lemma=update-orig-res-proof-and-extdelta! lemma-clause lemma-node named-lemma-formula res-proof extdelta-relation)
    
    (values new-res-proof
	    new-extdelta-relation
	    lemma-node
	    )))

(defun lemma=update-orig-res-proof-and-extdelta! (lemma-clause lemma-node named-lemma-formula res-proof extdelta-relation)
  (declare (edited  "11-MAY-1998")
	   (authors Ameier)
	   (input   "A lemma-clause, the according already produced lemma-node, the named lemma-formula,"
		    "the original resolution proof and the extdelta-relation.")
	   (effect  "In the original resolution proof the lemma-clause is justified as initial with delta-pairs"
		    "from the cnf of the lemma-formula with polarity t. This delta-pairs are added to the"
		    "delta-relation of the resolution proof and the extdelta-relation (with lemma-node as node)."
		    "On the other side the pairs of the initial-clauses that are not longer used (because lemma-clause"
		    "is justififed as initial-clause) are removed from the both delta-relations."
		    "In the resolution proof also the slot res~proof-clauses is updated (also because lemma-clause"
		    "is now justified as initial clause, what will cuase, that all steps above the lemma-clause"
		    "are cutted.")
	   (value   "Undefined."))
  (let* (;; normalize the closed lemma-node
	 (new-delta-relation (delta~create nil))
	 (new-lemma-cnf (keim::hocnf=normalize-named-formula named-lemma-formula
							     (res~proof-environment res-proof)
							     :delta-relation new-delta-relation
							     :pol 't))
	 (delta-relation (res~proof-delta-relation res-proof))
	 )
    
    ;; lemma-clause und new-lemma-clause sind gleich bis auf renaming, die neew-lemma-clause wird aber nicht eingesetzt sondern die
    ;; lemma-clause wird destructiv veraendert.

    ;; initial-justification
    (setf (node~justification lemma-clause) (res~initial-create))

    ;; in den neuen delta-relation pairs werden die neue-klausel durch die alte lemma-clause ersetzt
    (mapcar #'(lambda (pair)
		(setf (delta~delta-clause pair) lemma-clause))
	    (delta~relation-pairs new-delta-relation))
    
    ;; einsetzten der neuen delta-pairs 
    (setf (delta~relation-pairs delta-relation)
	  (append (delta~relation-pairs new-delta-relation)
		  (delta~relation-pairs delta-relation)))

    ;; setzten der clauses
    (setf (res~proof-clauses res-proof) (cons (res~proof-empty-clause res-proof)
					      (lemma=get-ancestors (res~proof-empty-clause res-proof))))

    ;; named-lemma-formula als zusaetzliche assumption
    (setf (res~proof-assumptions res-proof)
	  (cons named-lemma-formula (res~proof-assumptions res-proof)))
    (keim~put named-lemma-formula 'lemma 't)
    
    ;; updaten der extdelta-relation
    (mapcar #'(lambda (pair)
		(extdelta~add-pair! extdelta-relation
				    (delta~delta-formula pair)
				    (delta~delta-position-in-formula pair)
				    lemma-node
				    (delta~delta-clause pair)
				    (delta~delta-position-in-clause pair)))
	    (delta~relation-pairs new-delta-relation))
    
    ;; entfernt nicht mehr laenger benoetigte Sachen
    (lemma=minimize! res-proof extdelta-relation)
))

(defun lemma=minimize! (res-proof extdelta-relation)
  (declare (edited  "11-MAY-1998")
	   (authors Ameier)
	   (input   "A resolution proof and the according extdelta-relation.")
	   (effect  "The initial-clauses of the resolution proof are computed. All pairs"
		    "in the delta-relation of the resolution proof and the extdelta-relation"
		    "with clauses not in this initial-clauses are removed.")
	   (value   "Undefined."))
  (let* ((initial-clauses (res~proof-initial-clauses res-proof)))

    (setf (delta~relation-pairs (res~proof-delta-relation res-proof))
	  (remove-if-not #'(lambda (pair)
			     (find (delta~delta-clause pair) initial-clauses))
			 (delta~relation-pairs (res~proof-delta-relation res-proof))))

    (setf (delta~relation-pairs extdelta-relation)
	  (remove-if-not #'(lambda (pair)
			     (find (delta~delta-clause pair) initial-clauses))
			 (delta~relation-pairs extdelta-relation)))))

   

  
(defun lemma=compute-new-res-proof-and-extdelta (lemma-clause res-proof extdelta-relation)  
  (let* ((ancestors (lemma=get-ancestors lemma-clause))
	 (initial-ancestors (remove-if-not #'(lambda (ancestor)
					       (res~initial-p (node~justification ancestor)))
					   ancestors))
	 (conclusion (res~proof-conclusion res-proof))
	 (conclusion-node (pds~label2node (keim~name conclusion)))
	 (extdelta-pairs-of-initial-ancestors (remove-if-not #'(lambda (pair)
								 (find (extdelta~clause pair) initial-ancestors))
							     (delta~relation-pairs extdelta-relation)))
	 (nodes-of-pairs-of-initial-ancestors (remove-duplicates
					       (mapcar #'extdelta~pdsnode
						       extdelta-pairs-of-initial-ancestors)))
	 ;; produce a lemma-node in omega*current-proof-plan
	 (lemma-formula (lemma=construct-lemma-formula lemma-clause))
	 (sk-formulas (lemma=skolems-of-formula lemma-formula))
	 (lemma-name (pds~new-node-name))

	 ;; dass die conclusion-node nicht in den nodes-of-pairs-of-initial-ancestors ist muss vorher abgefangen werden
	 ;; siehe  lemma=insert-lemmata!
	 ;; falls einer der Knoten selbst ein lemma-knoten ist (in der plist 'lemma -> t) muessen fuer die Hyps seine Hyps
	 ;; genommen werden
	 ;; pro Skolem-konstante muss ein Dummy-Knoten erzeugt werden und bei den Hyps eingetragen werden. Wird spaeter die
	 ;; Skolem-Konstante ersetzt, so muss dieser Dummy-Knoten durch den dabei erzeugten Hyp-Knoten ersetzt werden.
	 ;; Der dummy-Knoten hat als Formel genau die Skolem-Konstante der er entspricht 
	 (hyp-nodes-for-lemma (remove-duplicates
			       (apply 'append
				      (cons (mapcar #'(lambda (skolem)
							(pdsn~make-hypothesis skolem (gensym "dummy-hyp")))
						    sk-formulas)
					    
					    (mapcar #'(lambda (node)
							(if (keim~get node 'lemma)
							    (pdsn~hyps node)
							  (list node)))
						    nodes-of-pairs-of-initial-ancestors)))))
	 
	 (lemma-node (pdsn~open-node-create lemma-formula hyp-nodes-for-lemma lemma-name))

	 ;; normalize the (open) lemma-node
	 (new-delta-relation (delta~create nil))
	 (new-env (env~create (res~proof-environment res-proof)))
	 (named-lemma-formula (termix~create-named-term lemma-name lemma-formula))
	 (lemma-cnf ;; each new created clause gets an initial justification
	  (mapcar #'(lambda (clause)
		      (let* ((new-initial (res~initial-create)))
			(setf (node~justification clause) new-initial))
		      clause)
		  (keim::hocnf=normalize-named-formula named-lemma-formula new-env
						       :delta-relation new-delta-relation
						       :pol nil)))
	 ;; compute the empty-clause of the new-resolution proof by resolving the lemma-clause and the cnf of the lemma-clause
	 (lemma-clause-copy (keim~copy lemma-clause
				       :downto '(data+primitive)))

	 ;; (new-empty-clause (do* ((rest-cnf lemma-cnf (rest rest-cnf))
	 ;;                         (current-end-clause lemma-clause-copy))
	 ;;		       ((null rest-cnf) current-end-clause)
	 ;; 		     (let* ((head-clause (first rest-cnf)))
	 ;;		       (setq current-end-clause (first (res~binary-resolution current-end-clause head-clause))))))

	 (new-empty-clause (lemma=new-empty-clause lemma-clause-copy lemma-cnf))

	 ;; compute the new-resolution proof
	 (named-formulas-of-pairs (remove-duplicates
				   (mapcar #'delta~delta-formula extdelta-pairs-of-initial-ancestors)))
	 (new-res-name (format nil "LEMMA-~A-~A" lemma-name (keim~name res-proof)))
	 (new-res-proof (res~proof-create new-res-name omega*current-proof-plan (prob~theory omega*current-proof-plan)
					  named-lemma-formula named-formulas-of-pairs
					  :env new-env))
	 ;; fehlende Teile des Resolution proofs 
	 (all-clauses (cons new-empty-clause (lemma=get-ancestors new-empty-clause))) ;; Vorsicht! Nicht geordnet !!!!!
	 (skolems (remove-duplicates
		   (apply 'append
			  (mapcar #'(lambda (clause)
				      (lemma=skolems-of-clause clause))
				  (remove-if-not #'(lambda (clause)
						     (res~initial-p (node~justification clause)))
						 all-clauses))))))
    ;; fuege lemma-node ein:
    (setf (pds~open-nodes omega*current-proof-plan)
	  (cons lemma-node (pds~open-nodes omega*current-proof-plan)))
    (pds~only-insert-node! lemma-node omega*current-proof-plan)
    ;; (omega~message "~%~%Inserting the node ~A: ~A ~%~%" lemma-node (node~formula lemma-node))
    (keim~put lemma-node 'lemma 't)
    
    ;; update die r2ntop*lemma-skolems
    (setq r2ntop*lemma-skolems
	  (remove-duplicates
	   (append r2ntop*lemma-skolems (lemma=skolems-of-formula lemma-formula))))
    
    ;; fuege auch die paare der anderen initial-ancestors in die delta-relation ein.
    (mapcar #'(lambda (extdelta-pair)
		(delta~add-pair! new-delta-relation
				 (extdelta~formula extdelta-pair)
				 (extdelta~formula-position extdelta-pair)
				 (extdelta~clause extdelta-pair)
				 (extdelta~clause-position extdelta-pair)))
	    extdelta-pairs-of-initial-ancestors)
    
    (setf (res~proof-empty-clause new-res-proof) new-empty-clause)
    (setf (res~proof-clauses new-res-proof) all-clauses)
    (setf (res~proof-skolem-functions new-res-proof) skolems)
    (setf (res~proof-delta-relation new-res-proof) new-delta-relation)

    (values new-res-proof
	    (extdelta~add-nodes-to-delta (cons lemma-node nodes-of-pairs-of-initial-ancestors) new-delta-relation) ;; the new extdelta
	    lemma-node
	    named-lemma-formula
	    )))

(defun lemma=new-empty-clause (lemma-clause lemma-cnf)
  (declare (edited  "12-MAY-1998")
	   (authors Ameier)
	   (input   "A clause (the lemma clause) and a list of clauses (contradictional clauses).")
	   (effect  "None.")
	   (value   "A empty clause: The result of resolving the lemma-clause with the"
		    "other clauses."))
  (let* ((unit-resolvents (if (> (length (cl~literals lemma-clause)) 1)
			      (res~ur-resolution lemma-clause (rest lemma-cnf))
			    (list lemma-clause)))
	 (head-clause (first lemma-cnf)))
    
    (first (apply 'append (mapcar #'(lambda (unit-clause)
				      (res~binary-resolution unit-clause head-clause))
				  unit-resolvents)))))

  
(defun lemma=construct-lemma-formula (clause)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "The lemma-formula:"
		    "The literals are connected by ORs and free-variables are replaced by FORALLs."))
  (let* ((literals (cl~literals clause))
	 (free-variables (data~free-variables clause))
	 (or-constant (logic~disjunction-constant))
	 (not-constant (logic~negation-constant))
	 (forall-constant (logic~universal-quantor :name 'forall))
	 (or-formula (do* ((rest-literals (rest literals) (rest rest-literals))
			   (current-formula (if (lit~positive-p (first literals))
						(data~copy (lit~atom (first literals))
							   :downto '(data+primitive))
					      (term~appl-create not-constant
								(list (data~copy (lit~atom (first literals))
										 :downto '(data+primitive)))))))
			 ((null rest-literals) current-formula)
		       (let* ((head-literal (first rest-literals)))
			 (setq current-formula (term~appl-create or-constant
								 (list current-formula 
								       (if (lit~positive-p head-literal)
									   (data~copy (lit~atom head-literal)
										      :downto '(data+primitive))
									 (term~appl-create not-constant
											   (list (data~copy (lit~atom head-literal)
													    :downto '(data+primitive))))))))))))
    
    
    (do* ((rest-free-variables free-variables (rest rest-free-variables))
	  (current-formula or-formula))
	((null rest-free-variables) current-formula)
      (let* ((head-free-variable (first rest-free-variables))
	     (abstr (term~abstr-create (list head-free-variable) current-formula)))
	(setq current-formula (term~appl-create forall-constant (list abstr)))))))

	
	       
#| ---------------------------------------------- Find lemma clauses ----------------------------------------------------------- |#

(defun lemma=get-lemma-clauses (step-clauses)
  (let* ((lemma-clauses (remove-if-not #'lemma=lemma-clause-p step-clauses)))
    (do* ((rest-clauses lemma-clauses (rest rest-clauses))
	  (back-clauses nil))
	((null rest-clauses)
	 back-clauses)
      (let* ((head-clause (first rest-clauses)))
	(setq back-clauses (lemma=insert-clause-into-lemma-list head-clause back-clauses))))))

	     
(defun lemma=lemma-clause-p (clause)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "Checks wether a clause should be used as lemma clause. If this is the case t is returned"
		    "otherwise nil."
		    "A clause is a lemma clause if:"
		    "1. It is no initial clause, consists of more than one literal and has more than one children !"
		    "2. It is no initial clause, consists of one literal and has more than one ground-reference !"
		    ))
   
  ;; Man beachte: Mehr als eine ground-referenz -> mehr als ein children !!
  ;; Der umkehr Schluss gilt zwar nicht, trotzdem approximieren wir die Anzahl der ground-referenzes mittels
  ;; der Anzahl der Children, da das Berechnen der ground-referenzes sehr (SEHR !) teuer sein kann, und wir
  ;; mittels der Lemmatisierung gerade solche Komplexitateten zerschlagen wollen !
  
  (let* ((just (node~justification clause))
	 (childrens (keim~get clause 'children)))
    (cond ((res~initial-p just)
	   nil)
	  ((cl~empty-p clause)
	   nil)
	  ((or (and (string-equal r2ntop*lemma 'constants)
		    (lemma=clause-contains-skolemfunction-p clause))
	       ;; flag constants forbids the use of skolemfunktions in lemmas!
	       (and (string-equal r2ntop*lemma 'free)
		    (lemma=clause-contains-skolemconstant-p clause))
	       ;; flag free forbids the use of any skolemterms in lemmas!
	       )	   
	   nil)
	  (t ;; for both units and non-unit clauses
	   
	   (if (> (length childrens) 1)
	       (if (lemma=self-tree-above-clause-is-big-enough-p clause)
		   't
		 nil)
	     nil)))))


(defun lemma=insert-clause-into-lemma-list (clause current-lemma-clauses)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause and the list of the current lemma-clauses.")
	   (effect  "None.")
	   (value   "The updated list of lemma-clauses, the clause is inserted at the right place in the"
		    "lemma-list, that means no DESCEDANT-CLAUSE before an ANCESTOR-CLAUSE."))    
  (if (find clause current-lemma-clauses)
      current-lemma-clauses
    ;; fuege clauses hinter ihren ancestors ein !
    (let* ((ancestors-of-clause (keim~get clause 'ancestors))
	   (ancestors-in-list (intersection ancestors-of-clause current-lemma-clauses)))
      (do* ((rest-in-list current-lemma-clauses (rest rest-in-list))
	    (rest-ancestors ancestors-in-list)
	    (out-list nil))
	  ((null rest-ancestors) (append out-list (list clause) rest-in-list)) 
	(let* ((head-in-list (first rest-in-list)))
	  
	  (when (find head-in-list rest-ancestors)
	    (setq rest-ancestors (remove head-in-list rest-ancestors)))
	  
	  (setq out-list (append out-list (list head-in-list))))))))


(defun lemma=self-tree-above-clause-is-big-enough-p (clause)
  (let* ((literals (cl~literals clause))
	 (variables (data~free-variables clause))
	 (weight-of-clause (+ (length literals) (length variables)))
	 (clause-just (node~justification clause))
	 (parents (res~justification-parents clause-just))
	 (children-number (length (keim~get clause 'children)))
	 (start-weight (if (or (res~hyper-resolution-p clause-just)
			       (res~ur-resolution-p clause-just))
			   (- (length (res~justification-parents clause-just)) 1)
			 1))
	 )
    (do* ((current-parent-level parents)
	  (current-initial-parents nil)
	  (current-weight-of-tree start-weight))
	((null current-parent-level)
	 (let* ((weight-of-tree (cond ((null (cl~unit-p clause))
				       current-weight-of-tree)
				      (t
				       (/ current-weight-of-tree 2))))
		(non-unit-initial-parents  (remove-if #'cl~unit-p current-initial-parents))
		(length-non-unit-initial-parents (length non-unit-initial-parents))
		(approximated-costs (cond ((null non-unit-initial-parents)
					   (* children-number weight-of-tree))
					  ((null (cl~unit-p clause))
					   (* children-number (/ (+ weight-of-tree (* 2 length-non-unit-initial-parents)) 3)))
					  (t
					   (* children-number (/ (+ weight-of-tree (* 4 length-non-unit-initial-parents)) 5)))))
		(quotient (cond ((and (null variables)
				      (cl~unit-p clause)
				      (null (lemma=skolems-of-clause clause))
				      )
				 ;; Unit-clause ohne skolems, ohne variables -> kein Aufwand -> fuehre immer ein !!
				 (+ r2ntop*lemma*relative-weight 1))
				(t
				 (/ approximated-costs weight-of-clause)))))
	   
	   ;; (format t "~%~%Examining clause ~A" clause)
	   ;; (format t "~%WEIGHT OF TREE IS ~A" weight-of-tree)
	   ;; (format t "~%LENGTH NON UNIT INITIAL PARENTS: ~A" length-non-unit-initial-parents)
	   ;; (format t "~%APROXIMATED COSTS: ~A" approximated-costs)
	   ;; (format t "~%QUOTIENT IS: ~A" quotient)
	   
	   (if (> quotient r2ntop*lemma*relative-weight)
	       't
	     nil)))
      (let* ((num-from-justs-of-current-parent-level (eval
						      (cons '+ (mapcar #'(lambda (clause)
									   (let* ((just (node~justification clause)))
									     (cond ((> (length (keim~get clause 'children)) 1)
										    ;; diese clause wird wohl selbst als lemma enden
										    ;; -> zaehlt hier nur noch ein bisserl mehr als eine
										    ;;    initial, ein bisserl mehr, weil nicht klar ist
										    ;;    ob sie wirklich als Lemma endet
										    1.5)
										   ((or (res~hyper-resolution-p just)
											(res~ur-resolution-p just))
										    (- (length (res~justification-parents just)) 1))
										   (t
										    1))))
								       current-parent-level)))
						     ;; jede just zaehlt 1 (ausser hyper + ur-resolution -> siehe oben)
						     ;; auch initial clauses zaehlen so 1, denn man muss davon ausgehen, dass auch sie
						     ;; arbeit machen durch instantiierungen etc.
						     )
	     (next-parent-level (apply 'append (mapcar #'(lambda (clause)
							   (let* ((just (node~justification clause)))
							     (cond ((typep just 'res+initial)
								    nil)
								   ((typep just 'res+reflex)
								    nil)
								   ((> (length (keim~get clause 'children)) 1)
								    ;; diese clause wird wohl selbst als lemma enden
								    ;; -> hier abbrechen
								    nil)
								   (t
								    (res~justification-parents just)))))
						       current-parent-level)))
	     (new-initial-parents (apply 'append (mapcar #'(lambda (clause)
							     (let* ((just (node~justification clause)))
							       (cond ((typep just 'res+initial)
								      (list clause))
								     ((typep just 'res+reflex)
								      (list clause))
								     ((> (length (keim~get clause 'children)) 1)
								      ;; diese clause wird wohl selbst als lemma enden
								      ;; -> hier als initial werten
								      (list clause))
								     (t
								      nil))))
							 current-parent-level))))
	
	(setq current-parent-level next-parent-level)
	(setq current-initial-parents (append current-initial-parents new-initial-parents))
	(setq current-weight-of-tree (+ current-weight-of-tree num-from-justs-of-current-parent-level))
	))))
;; Jeder Resolutions und jeder Factorisierungs Schritt entspricht 1
;; Hyper und Ur Resolution werden durch die ADD-NUM hoeher bewerted, da sie mehrere Resolutions Schritte ersetzen !!


(defun lemma=clause-contains-skolemfunction-p (clause)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "T if the clause contains a skolemfunction with more as zero arguments."))
  (let* ((skolems (lemma=skolems-of-clause clause)))
    (if (remove-if #'(lambda (sk)
		       (= (sksym~arity sk) 0))
		   skolems)
	't
      nil)))

(defun lemma=clause-contains-skolemconstant-p (clause)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "T if the clause contains a skolemconstant (this constant can have arity 0 or higher!)."))
  (lemma=skolems-of-clause clause))

#| ------------------------------------------------------ Auxiliaries ------------------------------------------------------------ |#


(defgeneric lemma=set-ancestors! (clause just)
  (:method (clause (just res+initial))
	   (keim~put clause 'ancestors nil)
	   nil)
  (:method (clause just)
	   (let* ((set-ancestors (keim~get clause 'ancestors)))
	     (if set-ancestors
		 set-ancestors
	       (let* ((parents (res~justification-parents just))
		      (get-ancestors (append parents
					     (remove-duplicates (apply 'append
								       (mapcar #'(lambda (parent)
										   (lemma=set-ancestors! parent (node~justification parent)))
									       parents))))))
		 (keim~put clause 'ancestors get-ancestors)
		 get-ancestors)))))

(defun lemma=set-children-clauses! (clause)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "Works on the hole tree above the clause and writes in the plist of each clause"
		    "an entry 'children, that contains their children clauses. Sets also in the plist"
		    "a flaf 'done, to sign that this branch was already checked.")
	   (value   "Undefined."))
  (let* ((just (node~justification clause))
	 (done (keim~get clause 'done)))
    
    (cond ((res~initial-p just)
	   nil)
	  (done
	   nil)
	  (t
	   (keim~put clause 'done 't)
	   (let* ((parents (res~justification-parents just)))
	     (mapcar #'(lambda (parent)
			 (keim~put parent 'children (remove-duplicates (cons clause (keim~get parent 'children))))
			 (lemma=set-children-clauses! parent))
		     parents)
	     )))))

(defun lemma=get-ancestors (clause)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "Computes the ancestors of the clause, yjis is the transitive"
		    "closure of the parent-relation."))
  (let* ((just (node~justification clause)))
    (if (res~initial-p just)
	nil
      (do* ((current-parents (res~justification-parents just))
	    (done-parents (res~justification-parents just))
	    (current-ancestors (res~justification-parents just)))
	  ((null current-parents)
	   (remove-duplicates current-ancestors))
	(let* ((next-parent-level (apply 'append (mapcar #'(lambda (parent)
							     (let* ((justi (node~justification parent)))
							       (if (res~initial-p justi)
								   nil
								 (res~justification-parents justi))))
							 current-parents)))
	       (next-parent-level-without-done-level (remove-if #'(lambda (next-parent)
								    (find next-parent done-parents))
								next-parent-level)))

	  (setq current-parents next-parent-level-without-done-level)
	  (setq done-parents (append done-parents next-parent-level-without-done-level))
	  (setq current-ancestors (append current-ancestors next-parent-level-without-done-level)))))))


(defun lemma=skolems-of-formula (formula)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A formula.")
	   (effect  "None.")
	   (value   "A list of all skolem-function of the formula."))
  (remove-duplicates
   (let* ((skolem-positions (data~positions formula #'sksym~sk-p)))
     (mapcar #'(lambda (pos)
		 (data~struct-at-position formula pos))
	     skolem-positions))))

(defun lemma=skolems-of-clause (clause)
  (declare (edited  "08-MAY-1998")
	   (authors Ameier)
	   (input   "A clause.")
	   (effect  "None.")
	   (value   "A list of all skolem-function of the clause."))
  (remove-duplicates (apply 'append (mapcar #'(lambda (literal)
						(let* ((atom (lit~atom literal)))
						  (lemma=skolems-of-formula atom)))
					    (cl~literals clause)))))


#| -------------------------------------------------- PULLEN DER EXISTS QUANTIFICATIONS ---------------------------------------------- |#

(defun lemma~pull-the-exists! (extdelta-relations)
  (declare (edited  "15-JAN-1999")
	   (authors Ameier)
	   (input   "A list of extdelta-relation.")
	   (effect  "For all skolem-constants in lemma-nodes (in r2ntop*lemma-skolems) the lines are computed in whose"
		    "CNF the skolem-constants are. On these nodes and the according positions of the quantifications in them"
		    "(which become the skolem-constants) the tactic pull-quantification is applied -> new lines in the ND-proof."
		    "Then the extdelta-relations are changed in a way that clauses former connected with unpulled line are"
		    "now connected with the new pulled line.") 
	   (value   "Undefined."))
  (let* ((all-pairs (apply 'append (mapcar #'delta~relation-pairs extdelta-relations))))
    
    ;; (format t "~%~%THIS ARE THE SKOLEM_CONSTANTS: ~A" r2ntop*lemma-skolems)
    
    (mapcar #'(lambda (skolem-const)
		
		;; (format t "~%~%SKOLEM-CONSTANT UNDER FIRE: ~A" skolem-const)
		
		(let* ((pair-thats-literal-contains-the-skolem-const
			(find skolem-const all-pairs :test #'(lambda (const pair)
							       (let* ((clause (extdelta~clause pair))
								      (clause-position (extdelta~clause-position pair))
								      (atom-at-position (data~struct-at-position clause
														 (pos~add-end
														  1
														  clause-position))))
								 (if (and (not (find (extdelta~pdsnode pair) r2ntop*lemma-nodes
										     :test #'eq))
									  (data~substruct-positions const atom-at-position))
								     't
								   nil)))))
		       (clause (extdelta~clause pair-thats-literal-contains-the-skolem-const))
		       (clause-position (extdelta~clause-position pair-thats-literal-contains-the-skolem-const))
		       (atom-at-position (data~struct-at-position clause (pos~add-end 1 clause-position)))
		       
		       (skolem-const-position (first (data~substruct-positions skolem-const atom-at-position)))
		       (pos-list (pos~number-list skolem-const-position))
		       (last-pos (first (last pos-list)))
		       (var-pos (if (or (null last-pos) 
					(= last-pos 0))
				    ;; -> sk selbst oder (sk x1 x2) usw. vergiss die letzte Null
				    (pos~list-position (reverse (rest (reverse pos-list))))
				  skolem-const-position))
		       
		       (formula-position (extdelta~formula-position pair-thats-literal-contains-the-skolem-const))
		       (formula (termix~term (extdelta~formula pair-thats-literal-contains-the-skolem-const)))
		       (atom-subformula (r2ntop~term-at-position formula formula-position))
		       (var-in-atom-subformula (r2ntop~term-at-position atom-subformula
									var-pos))
		       
		       (position-of-according-quant (lemma=position-of-according-quantification var-in-atom-subformula
												formula
												formula-position))
		       (unpulled-node (extdelta~pdsnode pair-thats-literal-contains-the-skolem-const))
		       (unpulled-node-without-scoped-equivs (lemma=expand-equivs! unpulled-node position-of-according-quant))
		       
		       (new-nodes (r2ntop~apply-tactic 'pull-quantification (list nil unpulled-node-without-scoped-equivs)
						       (list position-of-according-quant)))
		       (pulled-node (first new-nodes)))
		  
		  (lemma=update-extdeltas-by-pulled-node! extdelta-relations unpulled-node pulled-node position-of-according-quant)))
	    (remove-duplicates r2ntop*lemma-skolems))))

(defun lemma=expand-equivs! (node-with-equivs position-to-quantification)
  (declare (edited  "19-JAN-1999")
	   (authors Ameier)
	   (input   "A node and the position of a quntification in it.")
	   (effect  "It is possible that on the path to the quantification are equivalences."
		    "This equivalences are expanded (defn-expand), so the PDS is updated.")
	   (value   "The node with unexpanded equivalences."))

  (do* ((current-node node-with-equivs)
	(current-formula (node~formula current-node))
	(current-position-list nil (append current-position-list (list (first rest-pos-list))))
	(rest-pos-list (pos~number-list position-to-quantification) (rest rest-pos-list))
	(current-position (pos~list-position current-position-list) (pos~list-position current-position-list))
	(current-subformula (r2ntop~term-at-position current-formula current-position)
			    (r2ntop~term-at-position current-formula current-position))
	)
      ((null rest-pos-list)
       current-node)
    (progn

      ;; (format t "~%~%Curr-node: ~A" current-node)
      ;; (format t "~%Curr-formula: ~A" current-formula)
      ;; (format t "~%Curr-position-list: ~A" current-position-list)
      ;; (format t "~%Rest-pos-list: ~A" rest-pos-list)
      ;; (format t "~%Curr-position: ~A" current-position)
      ;; (format t "~%Curr-subformula: ~A" current-subformula)
      
      (when (logic~equivalence-p current-subformula)
	(let* ((equiv-const (env~lookup-object 'equiv (pds~environment omega*current-proof-plan)))
	       (definition (find equiv-const (th~definitions 'base) :test #'(lambda (const defin)
									      (data~equal const (th~definition-constant defin)))))
	       (definiendum (th~definition-constant definition))
	       (definiens (th~ass-node definition))
	       (new-nodes (r2ntop~apply-tactic 'defnE
					       (list nil current-node)
					       (list definiendum
						     definiens
						     (pos~add-end 0 current-position))))
	       (expanded-node (first new-nodes)))
	  
	  (setq current-node expanded-node)
	  (setq current-formula (node~formula current-node)))))))

(defun lemma=update-extdeltas-by-pulled-node! (extdelta-relations unpulled-node pulled-node position-of-according-quant)
  (declare (edited  "15-JAN-1999")
	   (authors Ameier)
	   (input   "A list of extdelta-relations, the node with unpulled-quantor, the node with pulled-quantor"
		    "the position of the pulled quantification.")
	   (effect  "The extdelta-relations are changed in a way that clauses former connected with the unpulled-quantor line"
		    "are then connected with the pulled-quantor line.")
	   (value   "Undefined."))
  (let* ((all-pairs (apply 'append (mapcar #'delta~relation-pairs extdelta-relations)))
	 (pos-length (length (pos~number-list position-of-according-quant))))

    (mapcar #'(lambda (pair)
		(let* ((node (extdelta~pdsnode pair)))
		  (if (null (eq node unpulled-node))
		      nil
		    (let* ((formula-position (extdelta~formula-position pair))
			   (formula-pos-length (length (pos~number-list formula-position)))
			   (new-pos (if (null (pos~prefix-p position-of-according-quant formula-position))
					(pos~add-front 1 (pos~add-front 0 formula-position))
				      (pos~add-front 1 (pos~add-front 0 (pos~concatenate position-of-according-quant  
											 (pos~last formula-position
												   (- formula-pos-length
												      (+ pos-length 2)))))))))

		      (setf (extdelta~delta-node-of-formula pair) pulled-node)
		      (setf (delta~delta-position-in-formula pair) new-pos)
		      (setf (delta~delta-formula pair) (termix~create-named-term (keim~name pulled-node)
										 (data~copy (node~formula pulled-node)
											    :downto '(data+primitive))))))))
	    all-pairs)))

(defun lemma=position-of-according-quantification (var formula path-position)
  (declare (edited  "15-JAN-1999")
	   (authors Ameier)
	   (input   "A variable, a formula and a position on whose path the quantification of the var has to be.")
	   (effect  "If the quantification is in the 'scope' of an equiv, this equiv is expanded.")
	   (value   "The position of the subformula in formula in that var is quantified."))
  
  (do* ((rest-pos-list (pos~number-list path-position) (reverse (rest (reverse rest-pos-list))))
	(curr-subformula (r2ntop~term-at-position formula (pos~list-position rest-pos-list))
			 (r2ntop~term-at-position formula (pos~list-position rest-pos-list))))
      
      ((or (null rest-pos-list)
	   (and (or (logic~universal-quantification-p curr-subformula)
		    (logic~existential-quantification-p curr-subformula))
		(eq var (logic~quantification-bound-variable curr-subformula))))
       
       (if (and (or (logic~universal-quantification-p curr-subformula)
		    (logic~existential-quantification-p curr-subformula))
		(eq var (logic~quantification-bound-variable curr-subformula)))
	   (pos~list-position rest-pos-list)
	 (error "~%Something goes wrong in lemma=position-of-according-quantification"))))) 
		
#| ---------------------------------------------------- COPY RES-PROOF-STEPS ------------------------------------------------------- |#

(defun lemma=copy-res-proof-steps! (res-proof)
  (declare (edited  "18-JAN-1999")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "All clauses in the steps of the resolution proof are copied (to separate them clearly to"
		    "the clauses of other resolution proofs). The delta-relation and the empty-clause slot are"
		    "also accordingly updated.")
	   (value   "The resolution proof itself."))

  (let* ((step-clauses (res~proof-step-clauses res-proof))
	 (initial-clauses (res~proof-initial-clauses res-proof))
	 (new-step-clauses (mapcar #'(lambda (clause)
				       (keim~copy clause :downto '(data+primitive)))
				   step-clauses))
	 (pairs-of-old-clauses-and-new-step-clauses (mapcar #'(lambda (old-clause new-clause)
								(list old-clause new-clause))
							    step-clauses new-step-clauses)))


    ;; update die parents slots der neuen step-clauses
    (mapcar #'(lambda (new-step-clause)
		(let* ((res-just (node~justification new-step-clause))
		       (parents (res~justification-parents res-just)))
		  
		  (when parents
		    (setf (res~justification-parents res-just)
			  (mapcar #'(lambda (old-parent)
				      (let* ((assoc-pair (assoc old-parent pairs-of-old-clauses-and-new-step-clauses :test #'eq)))
					(if assoc-pair
					    (second assoc-pair)
					  old-parent)))
				  parents)))))
	    new-step-clauses)

    ;; update empty-clause
    (setf (res~proof-empty-clause res-proof)
	  (second (assoc (res~proof-empty-clause res-proof)
			 pairs-of-old-clauses-and-new-step-clauses)))

    ;; update clauses
    (setf (res~proof-clauses res-proof)
	  (append initial-clauses new-step-clauses))
    
    ;; richtige Reihenfolge
    (lemma=order-resolution-steps! res-proof)
    
    res-proof))

(defun lemma=order-resolution-steps! (resolution-proof)
  (declare (edited  "09-DEC-1996")
	   (authors Ameier)
	   (input   "A resolution proof.")
	   (effect  "The sequence of steps in the proofs step slot is changed in a way,"
		    "that the last step is the empty clause and no new step stands before"
		    "its parents.")
	   (value   "The resolution proof."))
  (let* ((empty-clause (res~proof-empty-clause resolution-proof))
	 (initial-clauses (res~proof-initial-clauses resolution-proof))
	 (steps (lemma=get-ancs empty-clause (node~justification empty-clause) nil)))
    
    (mapcar #'(lambda (step)
		(keim~remprop step 'ancestors))
	    steps)
    
    (setf (res~proof-clauses resolution-proof) (append initial-clauses steps))
    resolution-proof))

(defgeneric lemma=get-ancs (clause just initials)
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
							   (lemma=get-ancs parent (node~justification parent) initials))
						       parents))
				(list clause))
			:from-end 't)))
		 (keim~put clause 'ancestors new-ancestors)
		 new-ancestors)))))

    
