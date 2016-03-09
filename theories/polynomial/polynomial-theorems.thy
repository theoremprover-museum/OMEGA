;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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

(th~deftheorem P-Add-Real1
	       (in polynomial)
	       (conclusion (forall (lam (p (num num))
			      (forall (lam (q (num num))
					    (= (p-plus-r1 p q)
					       (lam (z num)
						    (plus (p z) (q z)))))))))
	       )
(th~defsimplifier p-add-real1-simp 
		  (in polynomial)
		  (status global)
                  (equation p-add-real1)
                  (direction lr)
                  (help "Simplify polynomial sums"))

(th~deftheorem P-Times-Real1
	       (in polynomial)
	       (conclusion 
                (forall (lam (p (num num))
	         (forall (lam (q (num num))
		  (= (p-times-r1 p q)
	             (lam (z num) (times (p z) (q z))))))))))

(th~defsimplifier p-times-real1-simp 
		  (in polynomial)
		  (status global)
                  (equation p-times-real1)
                  (direction lr)
                  (help "Simplify polynomial products"))

(th~deftheorem S-Times-Real1
	       (in polynomial)
	       (conclusion (forall (lam (p (num num))
			     (forall (lam (s num)
				(= (s-times-r1 p s)
				   (lam (z num)
					(times s (p z)))))))))
	       )

(th~defsimplifier s-times-real1-simp 
		  (in polynomial)
		  (status global)
                  (equation s-times-real1)
                  (direction lr)
                  (help "Simplify polynomial s-multiplications"))

(th~deftheorem P-Diff-Real1
	       (in polynomial)
	       (conclusion (forall (lam (p (num num))
			       (forall (lam (q (num num))
				       (forall (lam (c num)
						     (= (p-deriv-r1 (p-plus-r1 p q) c)
							(p-plus-r1 (p-deriv-r1 p c) (p-deriv-r1 q c))))))))))
	       )

(th~defsimplifier p-diff-real1-simp 
		  (in polynomial)
		  (status global)
                  (equation p-diff-real1)
                  (direction lr)
                  (help "Simplify rational polynomials"))

(th~deftheorem Mon-Add-Real1
	       (in polynomial)
	       (conclusion  (forall (lam (X num)
					 (forall (lam (N num)
						      (forall (lam (A num)
								   (forall (lam (B num)
										(= (plus (times A (power X N))
											 (times B (power X N)))
										   (times (plus A B) (power X N))))))))))))
	       )

(th~defsimplifier Mon-Add-real1-simp 
		  (in polynomial)
		  (status global)
                  (equation mon-add-real1)
                  (direction lr)
                  (help "Simplify sums of monomials and polynomials."))


(th~deftheorem Mon-Multip-Real1
	       (in polynomial)
	       (conclusion  (forall (lam (X num)
					   (forall (lam (M num)
					   (forall (lam (N num)
						   (forall (lam (A num)
						   (forall (lam (B num)
								(= (times (times A (power X M))
									 (times B (power X N)))
								   (times (times A B) (power X (plus M N))))))))))))))))

(th~defsimplifier Mon-Multip-real1-simp 
		  (in polynomial)
		  (status global)
                  (equation Mon-Multip-real1)
                  (direction lr)
                  (help "Simplify products of monomials and polynomials."))
	     

(th~deftheorem Mon-Diff-Real1-1 
	       (in polynomial)
	       (conclusion (forall (lam (A num)
						(forall (lam (B num)
							     (= (p-deriv-r1 (lam (X num)
										 (times A (power X B)))
									    1)
								(lam (X num)
								     (times (times A B) (power X (minus B 1))))))))))
	       )

(th~defsimplifier Mon-Diff-real1-simp 
                  (in polynomial)
                  (status global)
                  (equation Mon-Diff-real1-1)
                  (direction lr)
                  (help "Simplify fractions of monomials and polynomials."))

(th~deftheorem Const-Diff-Real1-1 
	       (in polynomial)
	       (conclusion (forall (lam (A num)
					(= (p-deriv-r1 (lam (X num) (times A (power X 0)))
						       1)
					   (lam (X num) 0)))))
	       )

(th~deftheorem rolles-thm
              (in polynomial)
              (conclusion
               (forall (lam (a num)
                (forall (lam (b num)
                 (forall (lam (F (num num))
                  (implies (and (polynomial Real F)
                                (leq (degree F) 2))
                   (forall (lam (min num)
                    (implies (and (local-minimum min F)
                                  (and (greater (F a) (F min))
                                       (greater (F b) (F min))))
                             (total-minimum min F (closed-interval-bounds a b)))))))))))))
              (help "Rolles Theorem: For polynomials of degree leq 2, total minima on a 
                     closed interval I must be  either on the bounds of I or at a local minimum."))

(Th~deftheorem local-min-pol
               (in polynomial)
               (conclusion
                (forall (lam (F (num num))
                  (forall (lam (min num)
                    (implies (and (polynomial Real F)
                                  (and (= ((p-deriv-r1 F 1) min) 0)
                                       (greater ((p-deriv-r1 (p-deriv-r1 F 1) 1) min) 0)))
                             (local-minimum min F)))))))
                (help "A minimum theorem especially for univariate polynomials."))



