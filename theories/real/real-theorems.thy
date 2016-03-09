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

(th~deftheorem plus-real-step2
	       (in real)
	       (conclusion 
		(forall (lam (x num)
			     (forall (lam (y num)
					  (implies
					   (and (in x real) (in y real))
					   (= (plus x (s y)) (s (plus x y)))))))))
	       (help "The recursive definition of plus on the right term."))

(th~deftheorem plus-real-closed
	       (in real)
	       (conclusion (forall (lam (x num)
				     (forall (lam (y num)
						  (implies
						   (and (in x real) (in y real)) (in (plus x y) real))))))))


(th~deftheorem a-plus-real 
	       (in real)
	       (conclusion (forall (lam (x num)
				     (forall (lam (y num)
						  (forall (lam (z num)
							       (implies
								(and (in x real) (and (in y real) (in z real)))
								(= (plus (plus x y) z) (plus x (plus y z))))))))))))

(th~deftheorem c-plus-real
	       (in real)
	       (conclusion (forall (lam (x num)
				     (forall (lam (y num)
						  (implies
						   (and (in x real) (in y real))
						   (= (plus x y) (plus y x)))))))))

(th~deftheorem 0-plus-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(implies (in x real) (= (plus 0 x) x))))))

(th~deftheorem 1-times-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(implies (in x real) (= (times 1 x) x))))))

(th~deftheorem 0-times-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(implies (in x real) (= (times 0 x) 0))))))

(th~deftheorem a-times-real 
	       (in real)
	       (conclusion (forall (lam (x num)
				     (forall (lam (y num)
						  (forall (lam (z num)
							       (implies
								(and (in x real) (and (in y real) (in z real)))
								(= (times (times x y) z) (times x (times y z))))))))))))

(th~deftheorem c-times-real
	       (in real)
	       (conclusion (forall (lam (x num)
				     (forall (lam (y num)
						  (implies
						   (and (in x real) (in y real))
						   (= (times x y) (times y x)))))))))


(th~deftheorem Dist-Right-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(forall (lam (y num)
						     (forall (lam (z num)
								  (implies
								   (and (in x real) (and (in y real) (in z real)))
								   (= (times (plus x y) z)
								      (plus (times x z) (times y z))))))))))))

(th~deftheorem Dist-Left-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(forall (lam (y num)
						     (forall (lam (z num)
								  (implies
								   (and (in x real) (and (in y real) (in z real)))
								   (= (times x (plus y z))
								      (plus (times x y) (times x z))))))))))))

(th~deftheorem minus2plus
	       (in real)
	       (conclusion (forall (lam (x num)
                                        (forall (lam (y num)
					             (implies
                                                      (and (in x real) (in y real))
                                                      (= (minus x y) (plus x (times -1 y))))))))))

(th~deftheorem sqrt2power
	       (in real)
	       (conclusion (forall (lam (x num)
                                        (implies
                                         (in x real)
                                         (= (sqrt x) (power x 1/2)))))))

(th~deftheorem div2times
	       (in real)
	       (conclusion (forall (lam (x num)
                                        (forall (lam (y num)
					             (implies
                                                      (and (in x real) (in y real))
                                                      (= (div x y) (times x (divide 1 y))))))))))

(th~deftheorem power-1-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(implies (in x real) (= (power x 1) x))))))

(th~deftheorem 1-power-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(implies (in x real) (= (power 1 x) 1))))))

(th~deftheorem 0-power-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(implies (in x real) (= (power 0 x) 0))))))

(th~deftheorem power-0-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(implies (in x real) (= (power x 0) 1))))))

(th~deftheorem times-power-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(forall (lam (y num)
						     (forall (lam (z num)
								  (implies
								   (and (in x real) (and (in y real) (in z real)))
								   (= (power x (plus y z))
								      (times (power x y) (power x z))))))))))))

(th~deftheorem times-power-2-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(forall (lam (y num)
						     (forall (lam (z num)
								  (implies
								   (and (in x real) (and (in y real) (in z real)))
								   (= (power (times x y) z)
								      (times (power x z) (power y z))))))))))))

(th~deftheorem power-power-real
	       (in real)
	       (conclusion (forall (lam (x num)
					(forall (lam (y num)
						     (forall (lam (z num)
								  (implies
								   (and (in x real) (and (in y real) (in z real)))
								   (= (power (power x y) z)
								      (power x (times y z))))))))))))


(th~deftheorem rat-criterion
	       (in rational)
	       (conclusion
		(forall-sort (lam (x num)
				  (exists-sort (lam (y num)
				  (exists-sort (lam (z num)
						    (and (= (times x y) z)
							 (not (exists-sort (lam (d num)
										 (common-divisor y z d))
									   int))))
								 int))int))rat))
	       (help "x rational implies there exist integers y,z which have no common divisor with x=y*z."))






