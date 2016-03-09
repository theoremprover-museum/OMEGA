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

(th~defproblem matrix-plus-test
	       (in matrix)
	       (constants (a11 i)
			  (a12 i)
			  (a13 i)
			  (a21 i)
			  (a22 i)
			  (a23 i)
			  (a31 i)
			  (a32 i)
			  (a33 i)
			  (a44 i)
			  (a55 i)
			  (a66 i)
			  (a88 i)
			  (a77 i)
			  (a99 i)
			  (F (o i))
			  (fplus (i i i))
			  (ftimes (i i i)))
	       (assumption F (ring (set a11 a12 a13 a21 a22 a23 a31 a32 a33  a44 a55 a66 a77 a88 a99)       fplus ftimes))
	       (assumption S (subset
			      (set a11 a12 a13 a21 a22 a23 a31 a32 a33)
			      F))
	       (conclusion (= (matplus fplus
				       (mat(a11 a12 a13)
					   (a21 a22 a23)
					   (a31 a32 a33))
				       (transpose
					(mat(a11 a12 a13)
					    (a21 a22 a23)
					    (a31 a32 a33))))
			      (mat((fplus a11 a11)(fplus a12 a21)(fplus a13 a31))
				  ((fplus a21 a12)(fplus a22 a22)(fplus a23 a32))
				  ((fplus a31 a13)(fplus a32 a23)(fplus a33 a33))))))


(th~defproblem matrix-plus-integers
	       (in matrix)
	       (assumption a1 (ring int plus times))
	       (conclusion (= (matplus plus
				       (mat(11 12 13)
					   (21 22 23)
					   (31 32 33))
				       (mat(11 12 13)
					   (21 22 23)
					   (31 32 33)))
			      (mat
			       (22  24  26)  (42  44  46)  (62  64  66)))))

(th~defproblem matrix-mkm-concrete
	       (in matrix)
	       (constants (a11 i)
			  (a12 i)
			  (a13 i)
			  (a21 i)
			  (a22 i)
			  (a23 i)
			  (a31 i)
			  (a32 i)
			  (a33 i)
			  (a44 i)
			  (a55 i)
			  (a66 i)
			  (a88 i)
			  (a77 i)
			  (a99 i)
			  (aij (num num num))
			  (vi (num num))
			  (F (o i))
			  (fplus (i i i))
			  (ftimes (i i i)))
	       (assumption a1 (ring int plus times))
	       (conclusion (= (mattimes times plus
				       (mat (3  2  7)
					    (1  0  4))
				       (transpose (mat (3  2  7)
						       (1  0  4))))
			      (mat (62 31)
				   (31 17)))))

(th~defproblem matrix-transpose
	       (in matrix)
	       (assumption a1 (ring int plus times))
	       (conclusion (=  (transpose (transpose
					   (mat
					    (22  24  26)  (42  44  46)  (62  64  66))))
			       (mat
				(22  24  26)  (42  44  46)  (62  64  66)))))


(th~defproblem matrix-mkm-scalar
	       (in matrix)
	       (constants (a11 i)
			  (a12 i)
			  (a13 i)
			  (a21 i)
			  (a22 i)
			  (a23 i)
			  (a31 i)
			  (a32 i)
			  (a33 i)
			  (a44 i)
			  (a55 i)
			  (a66 i)
			  (a88 i)
			  (a77 i)
			  (a99 i)
			  (aij (num num num))
			  (vi (num num))
			  (F (o i))
			  (fplus (i i i))
			  (ftimes (i i i)))
	       (assumption a1 (ring int plus times))
	       (conclusion (= (scalartimes times 2
				       (mat (3  2  7)
					    (1  0  4)))
			      (mat (6 4 14)
				   (2 0 8)))))

(th~defproblem matrix-mkm-bloc
	       (in matrix)
	       (constants (a11 i)
			  (a12 i)
			  (a13 i)
			  (a21 i)
			  (a22 i)
			  (a23 i)
			  (a31 i)
			  (a32 i)
			  (a33 i)
			  (a44 i)
			  (a55 i)
			  (a66 i)
			  (a88 i)
			  (a77 i)
			  (a99 i)
			  (aij (num num num))
			  (bij (num num num))
			  (vi (num num))
			  (F (o i))
			  (fplus (i i i))
			  (ftimes (i i i)))
	       (assumption a1 (ring int plus times))
	       (conclusion (= (mattimes times plus
					  (bloc (mat (1))        (mat (2  3))
						(mat (0)(0))    aij)
					  (bloc (mat (1))        (mat (2  3))
						(mat (0)(0))    aij))
			      bij)))


(th~defproblem determinant-simple
	       (in matrix)
	       (constants (a num) (b num))
	       (conclusion (forall-sort (lam (n num)
					     (implies
					      (matrix (mat (a b hdots b) (b ddots ddots vdots) (vdots ddots ddots b) (b hdots b a)) n n real)
					      (= (determinant (mat (a b hdots b) (b ddots ddots vdots) (vdots ddots ddots b) (b hdots b a)))
						 (times (plus a (times (minus n 1) b)) (power (minus a b) (minus n 1))))))
					nat))
	       (help "A simple determinant problem."))
