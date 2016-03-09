;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

(th~deftheorem squeeze-theorem
               (in limit)
               (conclusion
                (forall (lam (g (num num))
                (forall (lam (a num)
                (forall (lam (l num)
                             (implies (exists (lam (f (num num))
                                      (exists (lam (h (num num))
                                      (and (forall (lam (x num)
                                                        (and (leq (f x) (g x))
                                                             (leq (g x) (h x)))))
                                           (and (lim f a l)
                                                (lim h a l)))))))
                                      (lim g a l)))))))))
	       (help "The squeeze theorem."))

(th~deftheorem absval-idempotence
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (= (absval (absval a))
				(absval a)))))
	       (help "The idempotence of |.| "))

(th~deftheorem absval-mult
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (= (absval (times a b))
					     (times (absval a)
						    (absval b))))))))
	       (help "The multiplicity of |.| ."))

(th~deftheorem absval-geq-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (leq 0 (absval a)))))
	       (help " 0 <= |.|"))

(th~deftheorem absval-great-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (implies (not (= 0 a))
				      (less 0 (absval a))))))
	       (help " 0 <> a => 0 < |a|."))

(th~deftheorem triangle-inequality
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(= a (plus b c))
							(leq (absval a)
							     (plus
							      (absval b)
							      (absval c)))))))))))
	       (help "The triangle inequality."))


(th~deftheorem less-additivity-left
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(less a b)
							(less (plus c a)
							      (plus c b))))))))))
	       (help "The additivity to the left of the <-relation."))

(th~deftheorem leq-additivity-left
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(leq a b)
							(leq (plus c a)
							     (plus c b))))))))))
	       (help "The additivity to the left of the <=-relation."))


(th~deftheorem less-additivity-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(less a b)
							(less (plus a c)
							      (plus b c))))))))))
	       (help "The additivity to the right of the <-relation."))

(th~deftheorem leq-additivity-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(leq a b)
							(leq (plus a c)
							     (plus b c))))))))))
	       (help "The additivity to the right of the <=-relation."))

(th~deftheorem less-multiplicity-left
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies (and (less a b)
								     (less 0 c))
								(less (times c a)
								      (times c b))))))))))
	       (help "The multiplicity to the left of the <-relation."))

(th~deftheorem leq-multiplicity-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies (and (leq a b)
								     (leq 0 c))
								(leq (times a c)
								     (times b c))))))))))
	       (help "The multiplicity to the right of the <=-relation."))

(th~deftheorem less-transitivity
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(and (less a b)
							     (less b c))
							(less a c)))))))))
	       (help "The transitivity of the <-relation."))

(th~deftheorem less-notequal
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies (less a b)
						   (not (= a b))))))))
	       (help "x < y => x <> y"))

(th~deftheorem div-r
	       (in limit)
	       (conclusion
		(forall (lam (x num)
			     (forall (lam (y num)
					  (implies (not (= 0 x))
						   (= y (div (times x y) x))))))))
	       (help "0 <> x => y = x*y/x."))

(th~deftheorem leq-transitivity
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(and (leq a b)
							     (leq b c))
							(leq a c)))))))))
	       (help "The transitivity of the <=-relation."))

(th~deftheorem leq-introduction
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies
					   (or (less a b)
					       (= a b))
					   (leq a b)))))))
	       (help "The transitivity of the <=-relation."))

(th~deftheorem leq-reflexivity
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (leq a a))))
	       (help "The reflexivity of the <=-relation."))


(th~deftheorem leq-less-transitivity
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(and (leq a b)
							     (less b c))
							(less a c)))))))))
	       (help "The transitivity of the <= and <-relation."))

(th~deftheorem less-leq-transitivity
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies
							(and (less a b)
							     (leq b c))
							(less a c)))))))))
	       (help "The transitivity of the < and <=-relation."))

(th~deftheorem less-is-leq
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies
					   (less a b)
					   (leq a b)))))))
	       (help "Less implies Leq."))

(th~deftheorem less-greater-equal
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (or (less a b)
					      (or (greater a b)
						  (= a b))))))))
	       (help "Quartium non datur."))

(th~deftheorem less2greater
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies (less a b)
						   (greater b a)))))))
	       (help " a<b --> b>a."))


(th~deftheorem minus-one-is-less
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (less (minus a 1) a))))
	       (help " a - 1 < a."))


(th~deftheorem plus-one-is-greater
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (less a (plus a 1)))))
	       (help " a < a + 1."))

(th~deftheorem div-two-greater-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (implies (less 0 a)
				      (less 0 (div a 2))))))
	       (help " 0<a --> 0<a/2 "))

(th~deftheorem two-greater-zero  ;; dummy 
	       (in limit)
	       (conclusion
		(less 0 2))
	       (help " 0<2 "))

(th~deftheorem fraction-greater-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies (and (less 0 a) (less 0 b))
						   (less 0 (div a b))))))))
	       (help " 0<a & 0<b --> 0<a/b "))

(th~deftheorem leq2geq
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies (leq a b)
						   (geq b a)))))))
	       (help " a<=b --> b>=a."))

;; see min-leq-l
;; (th~deftheorem min<=-l
;; 	       (in limit)
;; 	       (conclusion
;; 		(forall (lam (a num)
;; 			     (forall (lam (b num)
;; 					  (leq (min a b) a))))))
;; 	       (help "(min a b) <= a."))

;; see min-leq-r
;; (th~deftheorem min<=-r
;; 	       (in limit)
;; 	       (conclusion
;; 		(forall (lam (a num)
;; 			     (forall (lam (b num)
;; 					  (leq (min a b) b))))))
;; 	       (help "(min a b) <= b."))

(th~deftheorem minus-one-is-less
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (less (minus a 1) a))))
	       (help " a - 1 < a."))

(th~deftheorem plus-one-is-greater
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (less a (plus a 1)))))
	       (help " a < a + 1."))

(th~deftheorem div-two-greater-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (implies (less 0 a)
				      (less 0 (div a 2))))))
	       (help " 0<a ==> 0<a/2 "))

(th~deftheorem two-greater-zero  ;; dummy 
	       (in limit)
	       (conclusion
		(less 0 2))
	       (help " 0<2 "))

(th~deftheorem one-greater-zero  ;; dummy 
	       (in limit)
	       (conclusion
		(less 0 1))
	       (help " 0<1 "))

(th~deftheorem fraction-greater-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies (and (less 0 a) (less 0 b))
						   (less 0 (div a b))))))))
	       (help " 0<a /\ 0<b ==> 0<a/b "))

(th~deftheorem product-greater-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (implies (and (less 0 a) (less 0 b))
						   (less 0 (times a b))))))))
	       (help " 0<a /\ 0<b ==> 0<a*b "))

(th~deftheorem min-greater
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies (and (less c a) (less c b))
								(less c (min a b))))))))))
	       (help "Minimum introduction less."))

(th~deftheorem min-leq-left
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (leq (min a b) a))))))
	       (help "Minimum is smaller or equal than its arguments."))

(th~deftheorem min-leq-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (leq (min a b) b))))))
	       (help "Minimum is smaller or equal than its arguments."))


(th~deftheorem absval-geq-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (leq 0 (absval a)))))
	       (help "Absolute value is >= 0."))


(th~deftheorem min-geq
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies (and (leq c a) (leq c b))
								(leq c (min a b))))))))))
	       (help "Minimum introduction leq."))

(th~deftheorem max-smaller
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (implies (and (less a b) (less c b))
								(less (max a c) b)))))))))
	       (help "Maximum introduction."))

(th~deftheorem max-leq-left
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (leq a (max a b)))))))
	       (help "Maximum is greater or equal than its arguments."))

(th~deftheorem max-leq-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (leq b (max a b)))))))
	       (help "Maximum is greater or equal than its arguments."))

(th~deftheorem absval-geq-zero
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (leq 0 (absval a)))))
	       (help "Absolute value is >= 0."))


(th~deftheorem lim+-theorem
               (in limit)
               (conclusion (forall (lam (c num)
					(forall (lam (f (num num))
						     (forall (lam (g (num num))
								  (forall (lam (lf num)
									       (forall (lam (lg num)
											    (forall (lam (lf+g num)
													 (implies (and (and (lim f c lf)
															    (lim g c lg))
														       (= lf+g (plus lf lg)))
														  (lim (lam (x num)
															    (plus (f x) (g x)))
														       c
														       lf+g))))))))))))))))

(th~deftheorem lim--theorem
               (in limit)
               (conclusion (forall (lam (c num)
					(forall (lam (f (num num))
						     (forall (lam (g (num num))
								  (forall (lam (lf num)
									       (forall (lam (lg num)
											    (forall (lam (lf-g num)
													 (implies (and (lim f c lf)
														       (and (lim g c lg)
															    (= lf-g (minus lf lg))))
														  (lim (lam (x num)
															    (minus (f x) (g x)))
														       c
														       lf-g))))))))))))))))

	       
(th~deftheorem lim*-theorem
               (in limit)
               (conclusion (forall (lam (c num)
					(forall (lam (f (num num))
						     (forall (lam (g (num num))
								  (forall (lam (lf num)
									       (forall (lam (lg num)
											    (forall (lam (lf*g num)
													 (implies (and (lim f c lf)
														       (and (lim g c lg)
															    (= lf*g (times lf lg))))
														  (lim (lam (x num)
															    (times (f x) (g x)))
														       c
														       lf*g))))))))))))))))
	       

(th~deftheorem lim-const
               (in limit)
               (conclusion (forall (lam (c num)
					(forall (lam (const num)
						     (lim (lam (x num) const) c const)))))))
	       
(th~deftheorem lim-var
	       (in limit)
               (conclusion (forall (lam (c num)
					(lim (lam (x num) x) c c)))))
	       
(th~deftheorem sin1
	       (in limit)
	       (conclusion (forall (lam (x num)
					(leq (sin x) (absval x))))))

(th~deftheorem sin2
	       (in limit)
	       (conclusion (forall (lam (x num)
					(leq (minus 0 (absval x)) (sin x))))))

(th~deftheorem cos1
	       (in limit)
	       (conclusion (forall (lam (x num)
					(leq (cos x) 1)))))

(th~deftheorem cos2
	       (in limit)
	       (conclusion (forall (lam (x num)
					(leq (minus 1 (times (div 1 2) (times x x))) (cos x))))))


;;; Simplifiers (not used yet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

(th~defsimplifier absval-one
		  (in limit)
		  (status global)
                  (equation absval-one)
                  (direction lr)
                  (help "Simplify the absolute value of 1."))

(th~deftheorem one-times-num-left
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (= (times one a)
				a))))
	       (help "Multiplication by 1."))

(th~defsimplifier one-times-num-left
		  (in limit)
		  (status global)
                  (equation one-times-num-left)
                  (direction lr)
                  (help "Simplify the multiplication by 1."))

(th~deftheorem one-times-num-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (= (times a one)
				a))))
	       (help "Multiplication by 1."))

(th~defsimplifier one-times-num-right
		  (in limit)
		  (status global)
                  (equation one-times-num-right)
                  (direction lr)
                  (help "Simplify the multiplication by 1."))


(th~deftheorem add-zero-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (= (plus a 0)
				a))))
	       (help "Addition of 0."))


(th~defsimplifier add-zero-right
		  (in limit)
		  (status global)
                  (equation add-zero-right)
                  (direction lr)
                  (help "Simplify the addition of 0."))


;;;;;;;;;;;;;;

(th~deftheorem add-inverse-left
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
				     (= (plus (minus a b) b)
					a))))))
	       (help "Addition of the inverse."))


(th~defsimplifier add-inverse-left
		  (in limit)
		  (status global)
                  (equation add-inverse-left)
                  (direction lr)
                  (help "Simplify the addition of the inverse."))

(th~deftheorem add-minus-minus
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (= (plus (minus a b) (minus b c))
							  (minus a c)))))))))
	       (help "Addition of the inverse."))

(th~defsimplifier add-minus-minus
		  (in limit)
		  (status global)
                  (equation add-minus-minus)
                  (direction lr)
                  (help "Simplify the addition of the inverse."))

(th~deftheorem minus-to-plus
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (forall (lam (d num)
								    (= (plus (minus a b)
									     (minus c d))
								       (minus
									(plus a c)
									(plus b d))))))))))))
	       (help "Addition of the inverse."))

(th~defsimplifier minus-to-plus
		  (in limit)
		  (status global)
                  (equation minus-to-plus)
                  (direction lr)
                  (help "Simplify the addition of the inverse."))

;;;;;;
(th~deftheorem left-assoc-times-plus
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (= (times a (plus b c))
							  (plus (times a b)
								(times a c))))))))))
	       (help "Left-associativity of *."))


(th~defsimplifier left-assoc-times-plus
		  (in limit)
		  (status global)
                  (equation left-assoc-times-plus)
                  (direction lr)
                  (help "Simplify the left-associativity of *."))

;;;;;;;
(th~deftheorem left-assoc-times-minus
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (= (times a (minus b c))
							  (minus (times a b)
								 (times a c))))))))))
	       (help "Left-associativity of *."))


(th~defsimplifier left-assoc-times-minus
		  (in limit)
		  (status global)
                  (equation left-assoc-times-minus)
                  (direction lr)
                  (help "Simplify the left-associativity of *."))

;;;;
(th~deftheorem right-assoc-times-minus
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (forall (lam (c num)
						       (= (times (minus a b) c)
							  (minus (times a c)
								 (times b c))))))))))
	       (help "Left-associativity of *."))


(th~defsimplifier right-assoc-times-minus
		  (in limit)
		  (status global)
                  (equation right-assoc-times-minus)
                  (direction lr)
                  (help "Simplify the left-associativity of *."))

;;;;;;;;
;;;;;;;;
(th~deftheorem mult-inverse-right
	       (in limit)
	       (conclusion
		(forall (lam (a num)
			     (forall (lam (b num)
					  (= (times a (div b a))
					     b))))))
	       (help "Multiply the inverse."))


(th~defsimplifier mult-inverse-right
		  (in limit)
		  (status global)
                  (equation mult-inverse-right)
                  (direction lr)
                  (help "Simplify the multiplication of the inverse."))
|#
