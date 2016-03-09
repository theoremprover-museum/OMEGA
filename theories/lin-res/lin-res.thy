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

(th~deftheory lin-res
              (uses if-res-calc)
	      (help "The theory for propositional ground resolution."))

;(crule-applicable  (o    (o ee) glist crule))
;(crule-application (ee   (o ee) glist crule))
;(crule-result      (o ee (o ee) glist crule))
;


(th~defaxiom free-derivation-condition
	     (in lin-res)
	     (formula (forall (lam (d glist)
				   (= (free-derivation-cond d)
				      (forall (lam (it item)
					 (implies
					  (is-component it d)
					  (or (= (item2num (first (der-item-prems it)))
						 (minus (position it d) one))
					      (= (item2num (second (der-item-prems it)))
						 (minus (position it d) one))))))))))
	     (help "In the propositional Calculus no special condition for derivations"))
	     
(th~defaxiom res-applicable
	     (in lin-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			 (= (crule-applicable res-res seq S)
			    (resolvable-b (input-first seq) (input-second seq)
					  (input-third seq) (input-fourth seq))))))))
	     (help "A rule is applicable on a list of inputs if the inputs are resolvable."))
;; Diese Definition hat nicht mit der Liste die eine Ableitung ist zu tun.
;; Sie ist eldiglich auf eine Liste Definiert, damit man fuer alle Regeln mit
;; unterschiedlich vielen Praemissen nurt eine constante rule-applicable.. braucht.


(th~defaxiom res-application
	     (in lin-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form)))
			 (= (crule-application res-res seq S)
			    (resolvent-of (input-first seq) (input-second seq)
					  (input-third seq) (input-fourth seq))))))))
	     (help "What is the result of an res-res-rule application?"))
							    

(th~defaxiom res-result
	     (in lin-res)
	     (formula (forall (lam (seq glist) (forall (lam (S (o (o form))) (forall (lam (cl (o form))
			 (equiv (crule-result res-res seq S cl)
				(= (resolvent-of (input-first seq) (input-second seq)
						 (input-third seq) (input-fourth seq))
				   cl)))))))))
	     (help "Is a clause the result of the res crule?"))
