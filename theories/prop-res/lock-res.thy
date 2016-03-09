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


(th~deftheory lock-res
              (uses gebhard)
	      (constants (lock-res-start crule)
			 (lock-res-res   crule)
			 (crule-set (o crule))
			 (input-first  ((o form) glist))
			 (input-second ((o form) glist))
			 (input-third  (form     glist))
			 (input-fourth (form     glist))
			 (lix          (num   (o form)))   ;; lock-index
			 (lock-res-cond    (o  form (o form)))
			 )
	      (help "The theory for propositional lock-resolution."))

;(crule-applicable  (o    (o ee) glist crule))
;(crule-application (ee   (o ee) glist crule))
;(crule-result      (o ee (o ee) glist crule))

(th~defdef lock-res-cond
	   (in lock-res)
	   (formula (lam (cl (o form)) (lam (l form)
			 (and (cl l)
			      (forall (lam (li form)
			              (implies (cl li)
					       (leq (lix li) (lix l)))))))))
	   (help "One possibility to formalize the lock-condition."))


(th~defaxiom crule-set
  (in lock-res)
  (formula (forall (lam (x (crule))
			(equiv (in x crule-set)
			       (or (= x lock-res-start)
				   (= x lock-res-res)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  STUFF for the LOCK START RULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defaxiom  lock-start-applicable
	      (in lock-res)
	      (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			  (equiv (crule-applicable lock-res-start seq S)
				 (in (input-first seq) S)))))))
	      (help "The lock-res-start crule is applicable, if the premise clause is element of S."))


(th~defaxiom  lock-start-application
	      (in lock-res)
	      (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			   (= (crule-application lock-res-start seq S)
			      (input-first seq)))))))
	      (help "The conclusion of the lock start crule is its premise. Only the side condition of membership in S, has to be fullfilled."))

(th~defaxiom lock-start-result
	     (in lock-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form))) (forall (lam (cl (o form))
			 (equiv (crule-result lock-res-start seq S cl)
				(= (input-first seq) cl)))))))))
	     (help "The result of the lock start crule is its premise."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    STUFF for the LOCK RESOLUTION RULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defaxiom lock-res-applicable
	     (in lock-res)
	     (formula (forall (lam (input glist) (forall (lam (S (o (o form)))
			 (= (crule-applicable lock-res-res input S)
			    (and (and (lock-res-cond (input-first seq) (input-third seq))
				      (lock-res-cond (input-second seq) (input-fourth seq)))
			         (resolvable-b (input-first seq) (input-second seq)
					       (input-third seq) (input-fourth seq))))))))
	     (help "The lock rule is applicable on a list of inputs if the inputs are resolvable,"
		   "and the lock condition is true."))
;; Diese Definition hat nichts mit der Liste die eine Ableitung ist zu tun.
;; Sie ist eldiglich auf eine Liste Definiert, damit man fuer alle Regeln mit
;; unterschiedlich vielen Praemissen nur eine constante rule-applicable.. braucht.


(th~defaxiom lock-res-application
	     (in lock-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			 (= (crule-application lock-res-res seq S)
			    (resolvent-of (input-first seq) (input-second seq)
					  (input-third seq) (input-fourth seq))))))))
	     (help "What is the result of an lock-res-res-rule application?"
		   "The resolvant of th e clauses and literals."))
							    

(th~defaxiom lock-res-result
	     (in lock-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form))) (forall (lam (cl (o form))
			 (equiv (crule-result lock-res-res seq S cl)
				(= (resolvent-of (input-first seq) (input-second seq)
						 (input-third seq) (input-fourth seq))
				   cl)))))))))
	     (help "Is a clause the result of the res crule?"))
