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

(th~defproblem test
	       (in divisibility)
	       (author tintin)
	       (variables (x (o o)))
	       (conclusion conc (= x x))
)

(th~defproblem X5200
	       (in divisibility)
	       (author tintin)
	       (variables (x (all-types a (o a)))
			  (y (all-types a (o a))))
	       (reference "Andrews X5200 (p180)")
	       (conclusion conc
			   (all-types a
				      (= (union x y)
					 (union-over-collection
					  (lam (v (o a))
					       (or (= v x) (= v y))))))))

        
(th~defproblem simple
               (in divisibility)
	       (author tintin)
	       (variables (x (o)) (y (o)))
	       (conclusion conc (implies (and x y) x))
)	            

(th~defproblem simple2
               (in divisibility)
	       (author tintin)
	       (variables (x (o)) (y (o)))
	       (conclusion conc (equiv (implies x y) (or (not x) y)))
)	    

(th~defproblem simple3
               (in divisibility)
	       (author tintin)
               (variables (x (o)) (y (o)))
               (assumption ASS1 x)
	       (assumption ASS2 y)
	       (conclusion CONC (and x y))
)	       


(th~defproblem X5200-sub1
	       (in divisibility)
	       (author tintin)
	       (variables (x (all-types a (o a)))
			  (y (all-types a (o a)))
			  (a3 (all-types a (a))))
	       (reference "Andrews X5200 (p180)")
               (conclusion CONC (all-types a
					    (implies (or (x a3) (y a3))
						     (exists (lam (p (o a))
								  (and (or (= p x) (= p y)) 
								       (p a3))))))))


(th~defproblem X5200-sub2
	       (in divisibility)
	       (author tintin)
	       (variables  (x (all-types a (o a)))
			   (y (all-types a (o a)))
			   (a3 (all-types a (a)))) 
	       (reference "Andrews X5200 (p180)")
	       (assumption ASS  (all-types a (exists (lam (p (o a))
							  (and (or (= p x) (= p y)) (p a3))))))
	       
               (conclusion CONC (or (x a3) (y a3)))
)

(th~defproblem X5200-sub2-sub1
	       (in divisibility)
	       (author tintin)
	       (variables (x (all-types a (o a)))
			  (a6 (all-types a (o a)))
			  (a3 (all-types a (a))))
               (assumption ASS1 (= a6 x))
               (assumption ASS2 (a6 a3))
               (conclusion CONC (x a3))
               )


(th~defproblem X5209
	       (in divisibility)
	       (author tintin)
	       (reference "Andrews X5209 (p180)")
               (conclusion conc 
               	(all-types a (forall (lam (u (o a))
					  (forall (lam (v (o a))
						       (= (powerset (intersection u v))
							  (intersection (powerset u) (powerset v))))))))))

(th~defproblem X5208
	       (in divisibility)
	       (author tintin)
	       (reference "Andrews X5208 (p180)")
               (conclusion conc
               (forall (lam (p (o i))
               (forall (lam (q (o i))
                (equiv
                  (exists (lam (s (o i))
                              (forall (lam (x (i))
                                           (and
                                             (or (s x) (p x))
                                             (or (not (s x)) (q x))
                                           )
                                       )
                              )
                          )
                  )
                  (forall (lam (y(i))
                               (or (p y) (q y))
                          )
                  )
                )
               )))))
)

(th~defproblem existence
	       (in divisibility)
	       (author tintin)
               (conclusion 
               (forall (lam (x (o))
                       (exists (lam (y (o))
                               (equiv y (not x))
               )))))
)

(th~defproblem implication
	       (in divisibility)
	       (author tintin)
               (variables (a (o)) (b (o)) (c (o)))
               (assumption A1 (implies a (and b c)))
               (conclusion C1 (implies a b))
)

(th~defproblem one-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat one))
               (help "One is a natural number.")
)

(th~defproblem two-divides-four
               (in divisibility)
               (author tintin)
               (conclusion (divisor two four))
               (help "Two is a divisor of four.")
)

(th~defproblem two-times-two-equals-four
               (in divisibility)
               (author tintin)
               (conclusion (= (times 2 2) 4))
               (help "Two times two equals four.")
)

(th~defproblem divisor-and-mod
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (a num)
                   (forall-sort (lam (b num)
                     (equiv
                       (divisor b a)
                       (= (mod a b) 0)
                     )
                     ) int
                   )
                   ) int
                 )
               )
)

(th~defproblem gcd-equivalence
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (a num)
                   (forall-sort (lam (b num)
                     (forall-sort (lam (d num)
                       (equiv
                         (and
                           (and (divisor d a) (divisor d b))
                           (forall-sort (lam (c num)
                             (implies
                               (and (divisor c a) (divisor c b))
                               (divisor c d)
                             )
                           ) int)
                         )
                         (= (gcd a b) d)
                       )
                     ) int )
                   ) int)
                 ) int)
               )
)                         

(th~defproblem UB-problem
               (in divisibility)
               (author tintin)
               (conclusion 
                 (forall-sort (lam (y num)
                   (not (UB nat y)
                 )) nat)
               )
)

(th~defproblem successor-is-plus-one
               (in divisibility)
               (author tintin)
               (conclusion 
                  (forall-sort (lam (y num)
                                  (= (plus y 1) (s y))
                  ) nat )
               )
)

(th~defproblem not-prime-four
               (in divisibility)
               (author tintin)
               (conclusion
                  (not (prime 4))
               )
)

(th~defproblem two-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat two))
               (help "Two is a natural number.")
)

(th~defproblem three-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat three))
               (help "Three is a natural number.")
)

(th~defproblem four-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat four))
               (help "Four is a natural number.")
)

(th~defproblem five-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat five))
               (help "Five is a natural number.")
)

(th~defproblem six-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat six))
               (help "Six is a natural number.")
)

(th~defproblem seven-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat seven))
               (help "Seven is a natural number.")
)

(th~defproblem eight-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat eight))
               (help "Eight is a natural number.")
)

(th~defproblem nine-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat nine))
               (help "Nine is a natural number.")
)

(th~defproblem ten-nat
               (in divisibility)
               (author tintin)
               (conclusion (nat ten))
               (help "Ten is a natural number.")
)

(th~defproblem leq-and-equality
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (a num)
                   (forall-sort (lam (b num)
                     (equiv
                       (and (leq a b) (leq b a))
                       (= a b)
                     )
                   ) nat )
                 ) nat )
               )
               (help "for a and b nat: ((a leq b) and (b leq a)) is equivalent with (a = b)")
)

(th~defproblem leq-is-less-or-equal
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (a num)
                   (forall-sort (lam (b num)
                     (equiv
                       (leq a b)
                       (or (less a b) (= b a))
                     )
                   ) nat )
                 ) nat )
               )
               (help "for a and b nat: (a leq b) is equivalent with ((a < b) or (a = b))")
)

(th~defproblem one-neq-two
               (in divisibility)
               (author tintin)
               (conclusion
                 (not (= one two))
               )
)

(th~defproblem not-or
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall (lam (a o)
                   (forall (lam (b o)
                     (equiv
                       (not (or a b))
                       (and (not a) (not b))
                     )
                   ))
                 ))
               )
)

(th~defproblem leq-imp-succ-leq
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (x num)
                   (forall-sort (lam (y num)
                      (implies 
                        (leq x y)
                        (leq (s x) (s y))
                      )
                   ) nat)
                 ) nat)
               )
)

(th~defproblem extended-euclid-algorithm
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (a num)
                   (forall-sort (lam (b num)
                     (exists-sort (lam (x num)
                       (exists-sort (lam (y num)
                         (=
                           (gcd a b)
                           (plus (times x a) (times y b))
                         )
                       ) int )
                     ) int )
                   ) pos-nat )
                 ) pos-nat )
               )
)               

(th~defproblem division-with-remainder
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (a (num))
                   (forall-sort (lam (b (num))
                     (exists-unique-sort (lam (q (num))
                       (exists-unique-sort (lam (r (num))
                         (and
                           (= a (plus (times q b) r))
                           (less r b)
                         )
                       ) nat )
                     ) int )
                   ) pos-nat )
                 ) int )
               )
)              

(th~defproblem division-with-remainder-exists
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall-sort (lam (n (num))
                   (forall-sort (lam (a (num))
                     (forall-sort (lam (b (num))
                       (implies
                         (less a (plus b n))
                         (exists-sort (lam (q (num))
                           (exists-sort (lam (r (num))
                             (and
                               (= a (plus (times q b) r))
                               (less r b)
                             )
                          ) nat )
                         ) int )
                       )
                     ) pos-nat )
                   ) nat )
                 ) nat )
               )
)              

(th~defproblem test-nat-induct
               (in divisibility)
               (author tintin)
	       (constants (pred (o num)))
               (conclusion
                 (forall-sort (lam (x (num)) (pred x)) nat))

)              

(th~defproblem gcd-characterization-1
	       (in divisibility)
	       (author tintin)
               (conclusion
                 (forall-sort (lam (a num)
                   (forall-sort (lam (b num)
                     (forall (lam (d num)
                       (equiv
                         (= (gcd a b) d)
                         (and
                           (and (greater d 0) (common-divisor a b d))
                           (forall (lam (c num)
                             (implies
                               (common-divisor a b c)
                               (divisor c d)
                             )
                           ))
                         )
                       )
                     ))
                   ) pos-nat)
                 ) pos-nat)
               )
)              
                           
(th~defproblem product-prob 
	       (in divisibility)
	       (author tintin)
               (constants (f (num num)))
               (assumption function 
                 (= f (lam (x num) 1))
               )
               (conclusion
                 (forall-sort (lam (n num)
                   (= (product 0 n f) 1)
                 ) nat )
               )
)              

(th~defproblem test-that
               (in divisibility)
               (author tintin)
               (constants (c i))
               (conclusion
                 (forall (lam (x i) (= (that (lam (y i) (= y c))) x)))
               )             
)

(th~defproblem test-not-equal
               (in divisibility)
               (author tintin)
               (constants (c num)(b num))
               (conclusion
		(not (= 2 2)))
               )             


(th~defproblem infinite-primes
               (in divisibility)
               (author tintin)
               (conclusion
                 (not (finite prime))
               )
)

(th~defproblem test-thate
               (in divisibility)
               (author tintin)
               (constants (c i))
               (assumption a
                 (forall (lam (x i) (= (that (lam (y i) (= y c))) x)))
               )        
               (conclusion
                 (not (finite prime))
               )     
)

(th~defproblem xyzero
               (in divisibility)
               (author tintin)
               (constants (x num) (y num))
               (assumption a (= (plus x y) x))
               (conclusion (= y 0))
)

(th~defproblem add-to-equation
               (in divisibility)
               (author tintin)
               (conclusion
                 (forall (lam (a num)
                   (forall (lam (b num)
                     (forall (lam (c num)
                       (equiv (= a b) (= (plus a c) (plus b c)))
               )))))))
)

(th~defproblem prime-divisor
               (in divisibility)
               (author tintin)
               (conclusion
               (forall-sort (lam (n num)
                 (implies
                   (leq 2 n)
                   (exists-sort (lam (p num)
                     (and
                       (prime p)
                       (divisor p n)
                     )
                   ) nat )
                 )
               ) nat )
               )
)

(th~defproblem transitive-divisor
               (in divisibility)
               (author tintin)
               (conclusion (transitive divisor))
)

(th~defproblem not-prime-has
               (in divisibility)
               (author tintin)
               (conclusion
               (forall-sort (lam (n num)
                 (implies
                   (and
                     (leq 2 n)
                     (not (prime n))
                   )
                   (exists-sort (lam (p num)
                     (and
                       (less p n)
                       (divisor p n)
                     )
                   ) nat )
                 )
               ) nat )
               )
)

(th~defproblem exists-is-not-forall-not
               (in divisibility)
               (author tintin)
	       (constants (Pred (o num)))
               (conclusion
               (equiv
                 (exists (lam (x num) (Pred x)))
                 (not (forall (lam (x num) (not (Pred x)))))))
)

;(th~defproblem limitprob (in limit)
;
;               (conclusion thm
;                           (forall (lam (e num)
;                                  (exists (lam (d num)
;                                               (forall (lam (x num)
;                                                            (implies (less 0 e)
;                                                                     (and (less 0 d)
;                                                                          (implies
;                                                                           (and (less (absval (minus x 1)) d)
;                                                                                (greater (absval (minus x 1)) 0))
;                                                                           (less (absval (minus
;                                                                                          (div x (plus 1 x))
;                                                                                          (div 1 2)))
;                                                                                 e))))))))))
;                                    
;               ))
;
;(th~defproblem limit4.1.5 (in limit)
;
;                (constants (c (num))
;                           (f (num num)))
;
;               (conclusion thm
;               
;                  (forall (lam (l1 num)
;                     (forall (lam (l2 num)
;                        (implies
;                       
;                           (and
;                             (forall (lam (e num)
;                                 (exists (lam (d num)
;                                    (forall (lam (x num)
;                                       (implies
;                                          (less 0 e)
;                                          (and (less 0 d)
;                                               (implies
;                                                 (and
;                                                     (less (absval (minus x c)) d)
;                                                    (greater (absval (minus x c)) 0))
;                                                  (less (absval
;                                                     (minus (f x) l1))
;                                                 e))))))))))
;                        
;                             (forall (lam (e num)
;                                 (exists (lam (d num)
;                                    (forall (lam (x num)
;                                       (implies
;                                          (less 0 e)
;                                          (and (less 0 d)
;                                               (implies
;                                                 (and
;                                                     (less (absval (minus x c)) d)
;                                                    (greater (absval (minus x c)) 0))
;                                                  (less (absval
;                                                     (minus (f x) l2))
;                                                 e)))))))))))
;                         
;                           (= l1 l2))))))))
;
;(th~defproblem limit3.2.4 (in limit)
;
;                (constants (l (num))
;                           (x (num num)))
;                           
;                (assumption CONV
;                  (forall (lam (e1 num)
;                    (exists (lam (k1 num)
;                      (forall (lam (n1 num)
;                        (implies
;                          (greater e1 0)
;                          (implies
;                            (greater n1 k1)
;                            (less
;                              (absval
;                                (minus (x n1) l)
;                              )
;                              e1
;                            )
;                          )
;                        )
;                      ))
;                    ))
;                  ))
;                )
;                
;                (assumption GRZR
;                  (forall (lam (n num)
;                    (geq
;                      (x n)
;                      0
;                    )
;                  ))
;                )
;                
;                (conclusion LIM
;                  (geq l 0)
;                )
;)
;

