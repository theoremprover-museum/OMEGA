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


(th~deftheory taute-res
              (uses gebhard)
	      (constants (taute-res-start crule)
			 (taute-res-res   crule)
			 (crule-set (o crule))
			 (input-first  ((o form) glist))
			 (input-second ((o form) glist))
			 (input-third  (form     glist))
			 (input-fourth (form     glist))
			 (taute-res-cond    (o  (o form)))
			 )
	      (help "The theory for propositional tautology elemination resolution."))

;(crule-applicable  (o    (o ee) glist crule))
;(crule-application (ee   (o ee) glist crule))
;(crule-result      (o ee (o ee) glist crule))

(th~defdef taute-res-cond
	   (in taute-res)
	   (formula (lam (cl (o form)) 
			 (not (exists (lam (l1 form) (exists (lam (l2 form)
				 (and (and (cl l1) (cl l2))
				      (comp-pair l1 l2)))))))))
	   (help "One possibility to formalize the tautelemination-condition."))


(th~defaxiom crule-set
  (in taute-res)
  (formula (forall (lam (x (crule))
			(equiv (in x crule-set)
			       (or (= x taute-res-start)
				   (= x taute-res-res)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STUFF for the TAUTE START RULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defaxiom  taute-start-applicable
	      (in taute-res)
	      (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			  (equiv (crule-applicable taute-res-start seq S)
				 (and (taute-res-cond (input-first seq))
				      (in (input-first seq) S))))))))
	      (help "The taute-res-start crule is applicable, if the premise clause is element of S."
		    "And the premise is not a tautology."))


(th~defaxiom  taute-start-application
	      (in taute-res)
	      (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			   (= (crule-application taute-res-start seq S)
			      (input-first seq)))))))
	      (help "The conclusion of the taute start crule is its premise. Only the side condition of membership in S, has to be fullfilled."))

(th~defaxiom taute-start-result
	     (in taute-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form))) (forall (lam (cl (o form))
			 (equiv (crule-result taute-res-start seq S cl)
				(= (input-first seq) cl)))))))))
	     (help "The result of the taute start crule is its premise."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    STUFF for the TAUTE RESOLUTION RULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defaxiom taute-res-applicable
	     (in taute-res)
	     (formula (forall (lam (input glist) (forall (lam (S (o (o form)))
			 (= (crule-applicable taute-res-res input S)
			    (and (and (taute-res-cond (input-first seq))
				      (taute-res-cond (input-second seq)))
			         (resolvable-b (input-first seq) (input-second seq)
					       (input-third seq) (input-fourth seq))))))))
	     (help "The taute rule is applicable on a list of inputs if the inputs are resolvable,"
		   "and the taute condition is true."))
;; Diese Definition hat nichts mit der Liste die eine Ableitung ist zu tun.
;; Sie ist eldiglich auf eine Liste Definiert, damit man fuer alle Regeln mit
;; unterschiedlich vielen Praemissen nur eine constante rule-applicable.. braucht.


(th~defaxiom taute-res-application
	     (in taute-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			 (= (crule-application taute-res-res seq S)
			    (resolvent-of (input-first seq) (input-second seq)
					  (input-third seq) (input-fourth seq))))))))
	     (help "What is the result of an taute-res-res-rule application?"
		   "The resolvant of th e clauses and literals."))
							    

(th~defaxiom taute-res-result
	     (in taute-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form))) (forall (lam (cl (o form))
			 (equiv (crule-result taute-res-res seq S cl)
				(= (resolvent-of (input-first seq) (input-second seq)
						 (input-third seq) (input-fourth seq))
				   cl)))))))))
	     (help "Is a clause the result of the res crule?"))
