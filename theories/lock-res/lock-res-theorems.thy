;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1998 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@ags.uni-sb.de                                     ;;
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


;Beispiel aus LIMIT
;(th~deftheorem absval-idempotence
;               (in limit)
;               (conclusion
;                (forall (lam (a num)
;                             (= (absval (absval a))
;                                (absval a)))))
;               (help "The idempotence of |.| "))




(th~deftheorem fact1
	       (in lock-res)
	       (conclusion
		(forall (lam (x (o form)) (forall (lam (y (o (o form)))
				(implies (in x y)
					 (derivable x y crule-set)))))))
	       (help "Theorem for the fact, that every element of a set of clauses is derivable."))

(th~deftheorem fact2
               (in lock-res)
               (conclusion
                (forall (lam (y (o (o form)))
                            (implies (and (forall (lam (x (o form))
                                                       (implies (in x y) (setunit x))))
                                          (unsat-cl-set y))
                                     (derivable empty-cl y crule-set)))))
               (help "Theorem for the fact, that resolvable unit cluase, result in the empty clause."))

;(th~deftheorem fact2
;               (in lock-res)
;               (conclusion
;                (forall (lam (cs (o (o form)))
;                  (implies
;                   (exists (lam (x (o form))
;                           (exists (lam (y (o form))
;                                   (and (and (and (setunit x) (setunit y))
;                                             (resolvable-s x y))
;                                        (and (cs x) (cs y)))))))
;                   (derivable empty-cl cs crule-set))))))


(th~deftheorem fact3
	       (in lock-res)
	       (conclusion
		(forall (lam (klm (o (o form))) (forall (lam (gkl (o form))
                           (implies
                            (and (in klm all-clause-sets)
                                 (exists (lam  (kl1 (o form)) (and (klm kl1) (card>2 kl1)))))
                            (exists (lam (kl (o form)) (exists (lam (l form)
                                (and (and (implies
                                           (derivable
                                            gkl
                                            (union (setminus klm (singleton kl))
                                                   (singleton (setminus kl (singleton l))))
                                            crule-set)
                                           (or (derivable gkl klm crule-set)
                                               (derivable (union gkl (singleton l))
                                                          klm crule-set)))
                                          (kl l))
                                     (and (klm kl)
                                          (card>2 kl)))))))))))))
	       (help "A calculus independet formulation of the Disjunction Lemma"))


(th~defaxiom fact4
	     (in lock-res)
	     (formula (forall (lam (klm (o (o form))) (forall (lam (kl (o form))
			     (forall (lam (l form)
			       (implies
				(and
				 (and (klm kl) (kl l))
				 (and (derivable
				       (union empty-cl (singleton l))     
				       klm  
				       crule-set)
				      (derivable
				       empty-cl
				       (union (setminus klm (singleton kl))
					      (singleton (singleton l)))
				       crule-set)))
				 (derivable empty-cl klm crule-set)))))))))
	     (help "A formula helping to close the eln-technique application."))



(th~deftheorem leq-thm-1
	       (in lock-res)
	       (conclusion
		(forall (lam (x num) (forall (lam (y num)
			     (implies (and (nat x) (nat y))
				      (equiv (leq x y)
					      (or (= x y)
						  (leq (s x) y))))))))))

(th~deftheorem card2
	       (in lock-res)
	       (conclusion
		(forall (lam (as (o form)) 
			     (implies (card>2 as)
				      (exists (lam (el form)
						   (as el))))))))

