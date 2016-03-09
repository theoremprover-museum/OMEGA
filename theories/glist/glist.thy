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

;; Dies ist eine kleine Theorie, um ueber Listen und ihre wichttigsten eigenschaften reden
;; zu koennen. 

(th~deftheory glist
              (uses base integer)
	      (type-constants item glist );; Typkonstanten fuer genericlists
	      (constants ;; Konstanten fuer die generic lists
			 (NULL glist) 
			 (cons (glist glist item))
			 (first (item glist))
			 (second (item glist))
			 (tail (glist glist))
			 (num2item (item num))
			 (item2num (num item))
			 (append (glist glist glist))
			 (position (num glist item))
			 (is-component (o glist item))  
			 (last (item glist))			
			 (glist-finite (o glist))
			 (glist-length (num glist))
			 (yy2item     (all-types yy (item yy))) 
			 (item-object (all-types yy (yy item)))
			 (empty-glist (o glist))
			 (poslist2infolist (glist glist glist))
			 (glist-nth   (item glist num))
			 ;; (set-of-all-lists (o glist)) ;; Die Menge aller wirklichen Listen
			 ;; Konstanten fuer die theorie der Ableitungen 
			 )
	      (help "A test theory for completeness proofs."))


;; ===================================================================================
;;             GENERISCHE LISTEN und  ITEMS
;; ===================================================================================
(th~defaxiom first
	  (in glist)
	  (formula (forall (lam (x item) (forall (lam (L glist)
                      (= (first (cons x L)) x))))))
	  (help "Axiom defining the head accessor"))

;; so was wird fuer Vollstaendigkeitsbeweie nicht gebraucht,
;; da alle erforderlichen Zugriffsoperatoren nicht zwingend auf echten listen definiert
;; sein muessen.
;; Die Defnition von head wird nur beschrieben fuer cons-Ausdruecke. dies zu wissen
;; genuegt fuer Vollstaendigkeitsbeweise. Erste wenn man eigenschaften von cons beweisen
;; will sollte man diese unter der Vorraussetzung der Abgeschlossenheit unter
;; set-of-all-lists fuehren.

;(th~defaxiom list-base
;           (in glist)
;           (formula (in NULL set-of-all-lists))
;           (help "NULL ist eine wirklche Menge"))
;
;(th~defaxiom list-step
;             (in glist)
;             (formula
;              (forall (lam (x item) (forall (lam (L glist)
;                           (implies (in L set-of-all-lists)
;                                    (in
;                                     (cons x L)
;                                     set-of-all-lists))))))))
;
;(th~defaxiom list-induction
;             (in glist)
;             (type-variables aa)
;             (formula
;              (forall (lam (sol (o glist))
;                 (implies (and (sol NULL)
;                               (forall (lam (x item) (forall (lam (L glist)
;                                  (implies (in L sol)
;                                           (in (cons x L) sol)))))))
;                          (= sol set-of-all-lists))))))

	   

;(th~defaxiom NULL-A1
;          (in glist)
;          (formula (= (head NULL) NULL)))

(th~defaxiom NULL-A2
          (in glist)
          (formula (= (tail NULL) NULL)))

(th~defaxiom tail
           (in glist)
           (formula (forall (lam (x item) (forall (lam (L glist)
                      (= (tail (cons x L)) L))))))
	   (help "Axiom defining the tail accessor of a glist."))           

(th~defaxiom empty-glist
          (in glist)
	  (formula (forall (lam (L glist) (equiv (empty-glist L) (= L NULL)))))
	  (help "Axiom defining the empty  glist")) 

(th~defaxiom glist-finite
          (in glist)
	  (formula TRUE)
	  (help "Not yet implemented, ."))

(th~defaxiom item-object
          (in glist)
          (type-variables cc)
          (formula (forall (lam (ob cc)
                      (= (item-object (yy2item ob)) ob))))
          (help "Item object is the contents of the item"))

(th~defaxiom item-num
	   (in glist)
	   (formula (forall (lam (ob num)
                      (= (item-object (num2item ob)) ob))))
	   (help "Item object is the contents of the item"))


(th~defaxiom append-base1
	     (in glist)
	     (formula
	      (forall (lam (L glist)
			   (= (append NULL L)
			      L)))))

(th~defaxiom append-base2
	     (in glist)
	     (formula
	      (forall (lam (L glist)
			   (= (append L NULL)
			      L)))))

(th~defaxiom last-base
	     (in glist)
	     (formula
	      (forall (lam (x item)
			   (= (last (cons x NULL))
			      x)))))

(th~defaxiom last-step
	     (in glist)
	     (formula
	      (forall (lam (L glist) (forall (lam (x item)
		 (= (last (cons x L))
		    (last L))))))))


(th~defaxiom is-component-step
	     (in glist)
	     (formula
	      (forall (lam (L glist) (forall (lam (it item) (forall (lam (x item)
		 (= (is-component it (cons x L))
		    (or (= it x)
			(is-component it L)))))))))))

(th~defaxiom is-component-base
	     (in glist)
	     (formula
	      (forall (lam (x item)
		  (=  (is-component x NULL)
		      False)))))

;(th~defdef glist-nth
;           (in glist)
;           (definition
;             (lam (n num) (lam (l glist)
;                (num2item n)))))
