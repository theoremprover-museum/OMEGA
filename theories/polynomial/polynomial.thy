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

(th~deftheory polynomial
	      (uses calculus)
	      (constants
	       (ring-power (all-types bb (bb num bb (struct bb))))
	       (p-plus-r1 ((num num) (num num) (num num)))
	       (s-times-r1 ((num num) num (num num) ))
	       (p-plus-r2 ((num num num) (num num num) (num num num)))
	       (p-deriv-r1 ((num num) num (num num)))
	       (p-times-r1 ((num num) (num num) (num num))))
	      (help "Theory of elementary univariate polynomial calculus."))

; todo: redefine the following
;(th~defdef power
;           (in natural)
;           (type-variables bb)
;           (definition
;             (lam (n bb)
;                  (lam (m num)
;                       (iterate (ring-times n) m ring-one))))
;           (help "Exponentiation defined as iterated addition."))

;;;; die folgenden beiden Definitionen funktionieren so nicht!!!!  VS

;;(th~defdef p-plus-r1
;;           (in polynomial)
;;           (definition 
;;             (apply-pointwise-2 plus))
;;           (help "pointwise addition of functions."))
;;
;;(th~defdef s-times-r1
;;           (in polynomial)
;;           (definition 
;;             (apply-pointwise-2 times))
;;           (help "pointwise s-multiplication of functions."))

;(th~defdef monomial
;           (in polynomial)
;           (definition
;             (lam (S (struct num))
;                  (lam (F (num num))
;                       (exists (lam (exponent num)
;                                    (exists (lam (factor num)
;                                                 (and (and (in exponent nat) (in factor nat))
;                                                      (= F (lam (x num)
;                                                                (ring-times S factor (ring-power S x exponent))))))))))))
;           (help "Definition of monomials."))



;(th~defdef coefficients
;           (in polynomial)
;           (definition 
;             (lam (P (num num))
;                  (lam (n num)
;                       (nderivative n P zero))))
;           (help "The sequence of coefficients of a polynomial."))
;;  and what is this supposed to mean ????  VS

(th~defconstant degree
		(in polynomial)
		(type (num (num num))))

(th~defconstant polynomial
		(in polynomial)
		(type (o (num num)  (o num) )))

;(th~defdef degree
;           (in polynomial)
;           (definition 
;             (lam (F (num num))
;                    (minimum nat-plus-struct
;                             (lam (n num)
;                                  (and (not (= (coefficients F n) zero))
;                                       (forall (lam (m num)
;                                        (and (greater m n)
;                                             (= (coefficients F m) zero)))))))))
;           (help "Definition of degree of a polynomial as the minimal coefficient, such 
;                  that all greater ones are zero."))

;(th~defdef max-non-zero
;           (in polynomial)
;           (type-variables bb)
;           (definition
;             (lam (S (struct bb))
;                  (lam (Seq (bb num))
;                       (that (lam (n num)
;                                  (forall (lam (m num)
;                                               (implies (greater m n)
;                                                        (= (Seq m) (ring-zero S))))))))))
;           (help "(max-non-zero G) is the maximal number n, where (G n) is non-zero"))


;(th~defdef sum-till-n
;           (in polynomial)
;           (type-variables aa)
;           (definition
;           (lam (S (struct aa))
;                (lam (Seq (aa num))
;                     (lam (n num)
;                          ((iterate-warg
;                            (lam (i num)
;                                 (ring-plus S (Seq i)))
;                            n
;                            (ring-zero  S))))))))


;(th~defdef polynomial-with-coefficients
;           (in polynomial)
;           (type-variables bb)
;           (definition
;             (lam (S (struct bb))
;                  (lam (Seq (bb num))
;                       (lam (x bb)
;                            (sum-till-n
;                             S
;                             (lam (n num)
;                                  (ring-times  S (Seq n) (ring-power S x n)))
;                             (max-non-zero S Seq))))))
;           (help "The polynomial for a given set of coefficients."))

; (th~defdef polynomial
;            (in polynomial)
;            (type-variables bb)
;            (definition
;              (lam (S (struct bb))
;                   (lam (P (bb bb))
;                        (exists (lam (F (bb num))
;                                     (= P (polynomial-with-coefficients S F)))))))
;            (help "Definition of a polynomial function"))




