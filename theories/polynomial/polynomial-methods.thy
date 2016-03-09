;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
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


(infer~defmethod totmin-rolle
                 (outline-mappings (((existent)
				     totmin-rolle)))
                 (help "Finding a total minimum using Rolle's theorem"))


(meth~defmethod totmin-rolle totmin-rolle
                (in polynomial)
                (rating 10)
                (declarations
                 (sorted-meta-variables
                  (F (num num))
		  (fd-F (num num)) (sd-F (num num))
		  (fd-F-term (num num)) (sd-F-term (num num))
                  (min num)
                  (a num) (b num)
		  (x num var)))
		(premises )
		(application-condition
		 (mand (degree-polynomial F (:term 2))  ;;C1 
		       (mbind min (compute-pol-minimum F))  ;;C2
		       (mgreater (compute-pol-let F b) (compute-pol-let F min))  ;;C3
		       (mgreater (compute-pol-let F a) (compute-pol-let F min))  ;;C4
		       (mgreater-equal min a)  ;;C5
		       (mgreater-equal b min)))  ;;C6
                (expansion-computations
		 (pos1 (:position (1)))
		 (pos2 (:position (1 1)))
		 (fd-F (compute-with-cas (:term (p-deriv-r1 F 1))))
		 (sd-F (compute-with-cas (:term (p-deriv-r1 fd-F 1))))
		 (rolle (th-ass (:string "rolles-thm")))
		 (lmin (th-ass (:string "local-min-pol")))
		 (leq-symb (:symbol :leq-nat))
		 (gr-symb (:symbol :greater))
		 (=-symb (:symbol :=))
		 (poly-symb (:symbol :polynomial))
		 (the-cas (:symbol mycas)))
                (conclusions (- conc))
                (decl-content
                 (c1 () (polynomial Real F) ("Arith-Simplify" (poly-symb) ()))
                 (c2 () (leq (degree F) 2) ("Arith-Simplify" (leq-symb) ()))
		 (e1 () (= (fd-F min) 0) ("Arith-Simplify" (=-symb) ()))
                 (c3 () (= ((p-deriv-r1 F 1) min) 0) ("cas" (pos1 the-cas) (e1)))
		 (e3 () (greater (sd-F min) 0) ("Arith-Simplify" (gr-symb) ()))
		 (e2 () (greater ((p-deriv-r1 fd-F 1) min) 0) ("cas" (pos1 the-cas) (e3)))
		 (c4 () (greater ((p-deriv-r1 (p-deriv-r1 F 1) 1) min) 0) ("cas" (pos2 the-cas) (e2)))
                 (c5 () (greater (F a) (F min)) ("Arith-Simplify" (gr-symb) ()))
                 (c6 () (greater (F b) (F min)) ("Arith-Simplify" (gr-symb) ()))
                 (l2 (rolle lmin) (local-minimum min F) ("Assertion" () (lmin c1 c3 c4)))
		 (l1 (rolle lmin) (total-minimum min F (closed-interval-bounds a b)) ("Assertion" ()
										      (rolle c1 c2 l2 c5 c6)))
                 (conc () (exists (lam (x num)
				       (total-minimum x F (closed-interval-bounds a b)))) ("ExistsI" (min) (l1)))
                 )
                (proc-content schema-interpreter)
                (remark "The method applies a corollary to Rolles theorem, proving that a polynomial of degree has a total minimum in a given interval.")
                )
