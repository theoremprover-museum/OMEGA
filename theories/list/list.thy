;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
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

(th~deftheory list
              (uses integer)
	      (help "Theory of lists"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main constants like cons, first, rest, etc. are all generically
;; defined for list-like structures (e.g. lists, multi-sets,
;; cycles)and will be specialised once they are actually applied to
;; concrete structures.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defconstant cons
		(in list)
		(type (all-types aa ll (ll ll aa)))
		(help "Cons, the generic constructor of list-like structures."))
	     
(th~defconstant first    ;; replace by that defi
		(in list)
		(type (all-types aa ll (aa  ll)))
		(help "A generic constant specifying the first element of a list-like structure."))

(th~defaxiom first-defi
	     (in list)
	     (formula
	      (all-types aa ll 
			 (forall (lam (L ll)
				      (forall (lam (a aa) (= (first (cons a L)) a)))))))
	     (help "First element of a list."))

(th~defconstant rest   ;; replace by that defi
		(in list)
		(type (all-types aa ll (aa ll)))
		(help "A generic constant specifiying the rest of a list-like structure."))

(th~defaxiom rest-defi
	     (in list)
	     (formula
	      (all-types aa ll 
			 (forall (lam (L ll)
				      (forall (lam (a aa) (= (rest (cons a L)) L)))))))
	     (help "Rest of a list."))

(th~defconstant last     ;; replace by recursive defi
		(in list)
		(type (all-types aa ll (aa ll)))
		(help "Last element of a list."))

(th~defconstant length   ;; replace by recursive defi
		(in list)
		(type (all-types ll (num ll)))
		(help "The length of a list."))

(th~defconstant nth
		(in list)
		(type (all-types aa ll (aa ll num)))
		(help "The nth element of a list."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Stuff that is relevant for the concrete datastructure of lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(th~defconstant nil
		(in list)
		(type  list)
		(help "The empty list."))

(th~defaxiom last-base
	     (in list)
	     (formula
	      (forall (lam (a num) (= (last (cons a nil)) a))))
	     (help "Last element of a list."))

(th~defaxiom last-step
	     (in list)
	     (formula
	      (forall-sort (lam (L list)
				(forall (lam (a num)
					     (= (last (cons a L)) (last L)))))
			   (lam (x list)(not (= x nil)))))							  
	     (help "Last element of a list."))

(th~defaxiom length-base
	     (in list)
	     (formula
	      (= (length nil) 0))
	     (help "Length of the empty list is 0."))

(th~defaxiom length-step
	     (in list)
	     (formula
	      (all-types aa 
			 (forall-sort (lam (L list)
					   (forall (lam (a aa)
							(= (length (cons a L)) (plus (length L) 1)))))
				      (lam (x list)(not (= x nil))))))							  
	     (help "Length of list (a L) = 1 + Length of list L."))

