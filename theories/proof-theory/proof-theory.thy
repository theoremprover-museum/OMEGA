;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
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

(th~deftheory proof-theory
              (uses propositional-logic glist)
	      (type-constants crule)  ;; Typkonstante fuer Kalkuelregeln
	      (constants ;; Regelanwendung
			 (crule-applicable  (o     (o (o form)) glist crule))
			 (crule-application ((o form)  (o (o form)) glist crule))
			 (crule-result      (o (o form) (o (o form)) glist crule))
			 ;; Konstanten fuer die Ableitungen 
			 (der-item-constr (item glist glist (o form) crule))
			 ;;                       /    |      \      \
			 ;;                   links  inputs klausel regel
			 ;; Ein Ableitungitem enthaelt alle Wichtigen Informationen, die
			 ;; fuer die Entstehung des eigentlichen Item Objektes/Inhalts
			 ;; wichtig sind.
			 ;;     Indizees der Listenelemente der Eingabeformeln
			 ;;     Eigentliche Inhalt gespeichert (eee)
			 ;; KEINE Inputs: Bsp-weise Formel fuer die Startregel,
			 ;; Informationen auf welchen Literalen resolviert wurde werden im
			 ;; Moment noch als verzichtbar betrachtet.
			 ;; Wollte man die Theorie erweitern auf andere Kalkuele koennte
			 ;; es erforderlich sein die Definitionen zu erweitern, ohne das
			 ;; bestehende zu veraendern
			 
			 (der-item-info   ((o form) item))
			 (der-item-crule  (crule item))
			 (der-item-prems  (glist item))
			 (der-item-inp    (glist item))
			 (push-idx        (glist num glist)) ;; Genau die Funktion, die
							     ;; die Indizes, die die
							     ;; Praemissen der Klausel
							     ;; bestimmen um eine
							     ;; konstante Zahl vc erhoeht.
			 (cl2item         (item (o form)))
			 (form2item       (item form))
			 (item2cl         ((o form) item))
			 (item2form       (form item))
			 (idx-minus       (item item num))
			 ;; (prem-elems      (glist glist))
			 (prems-first     ((o form) glist))
			 (free-derivation-cond (o glist))
			 )
	      (help "A theory for proof-theory staff."))

;; ===================================================================================
;;   Kalkuelregeln
;;====================================================================================
;; Kalkuel regeln werden immer auf eine Menge von Urelementen angewandt
;; Der Input (praemissen) ist immer eine Liste
;; Die konklusion ist das Ergebniss der regelanwendung, die auf dem Input arbeitet
;; Ausserdem hat jede Regel noch eine Bedingung, die ueber den Input hinaus noch
;; zusaetzliche informationen als input nehmen kann
;; Daraus lassen dich dann die Funktionskonstanten wie applicable, apllication und
;; rule result definieren. 

;; ===================================================================================
;;   Generische Ableitungen 
;;====================================================================================

(th~defaxiom poslist2infolist
	     (in proof-theory)
	     (formula (forall (lam (x glist)
				   (=  (POSLIST2INFOLIST NULL x)
				       NULL)))))

(th~defaxiom cl2item-def
	     (in proof-theory)
	     (formula (forall (lam (akl (o form))
			    (= (item2cl (cl2item akl))
			       akl))))) 

;; Anpasssungen an die schlechten Listen
(th~defaxiom der-item-info
              (in proof-theory)
	      (type-variables eee)
              (formula  (forall (lam (ru crule) (forall (lam (el (o form))
			   (forall (lam (pr glist) (forall (lam (inp glist) 
	                        (= (der-item-info (der-item-constr ru el pr inp))
				   el))))))))))
	      (help "The info of an item is the resulting formula or clause."))


(th~defaxiom der-item-crule
              (in proof-theory)
	      (type-variables eee)
              (formula (forall (lam (ru crule) (forall (lam (el (o form))
			  (forall (lam (pr glist) (forall (lam (inp glist) 
	                     (= (der-item-crule (der-item-constr ru el pr inp))
				ru))))))))))
	      (help "The crule is the rule, that was applied the receive the info."))

(th~defaxiom der-item-prems
              (in proof-theory)
	      (type-variables eee)
              (formula (forall (lam (ru crule) (forall (lam (el (o form))
			  (forall (lam (pr glist) (forall (lam (inp glist) 
	                     (= (der-item-prems (der-item-constr ru el pr inp))
				pr))))))))))
	      (help "List of the Premises of the rule application"))


(th~defaxiom der-item-inp
             (in proof-theory)
	     (type-variables eee)
             (formula (forall (lam (ru crule) (forall (lam (el (o form))
			 (forall (lam (pr glist) (forall (lam (inp glist) 
	                    (= (der-item-inp (der-item-constr ru el pr inp))
			       inp))))))))))
	     (help "The input list contains all information, to determine the applied ruleinstance."))


(th~defdef derivation
	   (in proof-theory)
	   (type-variables eee)
	   (definition
	     (lam (S (o (o form))) (lam (crules (o crule)) (lam (seq glist)  
		  (forall (lam (x item) 
			  (implies (is-component x seq)
				   (exists (lam (r crule)
				    (and (free-derivation-cond seq)		
				       (and (and (crules r)
						 (crule-applicable r
						       (append (append
								(der-item-inp x)
								(poslist2infolist (der-item-prems x) seq))
							       (cons (cl2item (der-item-info x))
								     NULL))
						       S))
					    (and (forall (lam (y num)
						    (implies (is-component (num2item y) (der-item-prems x))
							     (less y
								   (position x seq)))))
						 (= (der-item-info x)		
						    (crule-application r
							 (append (append
								  (der-item-inp x)
								  (poslist2infolist (der-item-prems x) seq))
								 (cons (cl2item
									(der-item-info x))
								       NULL))
							 S))))))))))))))
	   (help "Derivation with respect to a set of crules and a set of initial
elements"))

(th~defdef derivation  ;; Die gleiche Definition wie oben, nur, dass die existenz der
		       ;; Regeln nicht gefordert wird, da sie in den items enthalten ist.
	   (in proof-theory)
	   (type-variables eee)
	   (definition
	     (lam (S (o (o form))) (lam (crules (o crule)) (lam (seq glist)  
		  (forall (lam (x item) 
			  (implies (is-component x seq)
				   (and (free-derivation-cond seq)		
				       (and (and (crules (der-item-crule x))
						 (crule-applicable (der-item-crule x)
						       (append (append
								(der-item-inp x)
								(poslist2infolist (der-item-prems x) seq))
							       (cons (cl2item (der-item-info x))
								     NULL))
						       S))
					    (and (forall (lam (y num)
						    (implies (is-component (num2item y) (der-item-prems x))
							     (less y
								   (position x seq)))))
						 (= (der-item-info x)		
						    (crule-application (der-item-crule x)
							 (append (append
								  (der-item-inp x)
								  (poslist2infolist (der-item-prems x) seq))
								 (cons (cl2item
									(der-item-info x))
								       NULL))
							 S))))))))))))
	   (help "Derivation with respect to a set of crules and a set of initial elements"))

(th~defdef derivation-of 
	   (in proof-theory)
	   (type-variables eee)
	   (definition
	     (lam (S (o (o form))) (lam (R (o crule)) (lam (A glist) (lam (d (o form))
		  (and (= (der-item-info (last A)) d)
			    (derivation S R A)))))))
	   (help " What is a derivation of an element"))

(th~defdef derivable
	   (in proof-theory)
	   (type-variables eee)
	   (definition
	     (lam (d (o form)) (lam (S (o (o form))) (lam (R (o crule))
					      (exists (lam (seq glist)
							   (derivation-of S R seq d)))))))
	   (help "What means derivable")) 


	                     
