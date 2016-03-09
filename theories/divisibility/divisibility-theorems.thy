;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
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


(th~deftheorem X5208 
(in DIVISIBILITY) 
(category THEOREM) 
(conclusion (FORALL (lam (P (O I)) (FORALL (lam (Q (O I)) (EQUIV (EXISTS (lam (S (O I)) (FORALL (lam (X I) (AND (OR (S X) (P X)) (OR (NOT (S X)) (Q X))))))) (FORALL (lam (Y I) (OR (P Y) (Q Y))))))))))
(help ""))

(th~deftheorem ONE-NAT 
(in DIVISIBILITY) 
(category THEOREM) 
(conclusion (NAT ONE))
(help "One is a natural number. "))

(th~deftheorem TWO-NAT 
(in DIVISIBILITY) 
(category THEOREM) 
(conclusion (NAT TWO))
(help "Two is a natural number. "))

(th~deftheorem EXTENDED-EUCLID-ALGORITHM 
(in DIVISIBILITY) 
(category THEOREM) 
(conclusion (FORALL-SORT (lam (A NUM) (FORALL-SORT (lam (B NUM)
               (EXISTS-SORT (lam (X NUM) (EXISTS-SORT (lam (Y NUM)
                 (= (GCD A B) (PLUS (TIMES X A) (TIMES Y B)))) INT)) INT))
                POS-NAT)) POS-NAT))
(help "Results of the extended euclid algorithm."))

(th~deftheorem times-nat-base
               (in divisibility)
               (conclusion
                (forall (lam (x num)
                             (implies (in x nat)
                                      (= (times x zero) zero)))))
               (help "Base case for the recursive definition of multiplication.")
)               

(th~deftheorem successor-is-plus-one
               (in divisibility)
               (conclusion 
                  (forall-sort (lam (y num)
                                  (= (plus y 1) (s y))
                  ) nat )
               )
               (help "s n = n + 1")
)               

(th~deftheorem PRIME-DIVISOR 
(in DIVISIBILITY) 
(conclusion (FORALL-SORT (lam (N NUM) (IMPLIES (LEQ 2 N) (EXISTS-SORT (lam (P NUM) (AND (PRIME P) (DIVISOR P N))) NAT))) NAT))
(help ""))

(th~deftheorem transitive-divisor
               (in divisibility)
               (conclusion (transitive divisor))
               (help "divisor is transitive")
)

