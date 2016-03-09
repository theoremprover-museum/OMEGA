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
              (uses if-res-calc)
	      (constants (lidx (num form (o form))))
	      (help "The theory for propositional lock-resolution."))

;(crule-applicable  (o    (o ee) glist crule))
;(crule-application (ee   (o ee) glist crule))
;(crule-result      (o ee (o ee) glist crule))

(th~defdef lock-res-cond
	   (in lock-res)
	   (definition (lam (cl (o form)) (lam (l form)
			 (and (cl l)
			      (forall (lam (li form)
			              (implies (cl li)
					       (leq (lidx cl li) (lidx cl l)))))))))
	   (help "One possibility to formalize the lock-condition."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        the LOCK RESOLUTION RULE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defaxiom res-applicable
	     (in lock-res)
	     (formula (forall (lam (S (o (o form))) (forall (lam (seq glist)
			 (= (crule-applicable res-res seq S)
			    (and (and (lock-res-cond (input-first seq) (input-third seq))
				      (lock-res-cond (input-second seq) (input-fourth seq)))
			         (resolvable-b (input-first seq) (input-second seq)
					       (input-third seq) (input-fourth seq)))))))))
	     (help "The lock rule is applicable on a list of inputs if the inputs are resolvable,"
		   "and the lock condition is true."))
;; Diese Definition hat nichts mit der Liste die eine Ableitung ist zu tun.
;; Sie ist eldiglich auf eine Liste Definiert, damit man fuer alle Regeln mit
;; unterschiedlich vielen Praemissen nur eine constante rule-applicable.. braucht.


(th~defaxiom res-application
	     (in lock-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			 (= (crule-application res-res seq S)
			    (resolvent-of (input-first seq) (input-second seq)
					  (input-third seq) (input-fourth seq))))))))
	     (help "What is the result of an res-res-rule application?"
		   "The resolvant of th e clauses and literals."))
							    

(th~defaxiom res-result
	     (in lock-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form))) (forall (lam (cl (o form))
			 (equiv (crule-result res-res seq S cl)
				(= (resolvent-of (input-first seq) (input-second seq)
						 (input-third seq) (input-fourth seq))
				   cl)))))))))
	     (help "Is a clause the result of the res crule?"))
