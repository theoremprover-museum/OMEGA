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

(th~deftheory LIMIT
	      (uses polynomial)
	      (constants 
	       (focus (o o))
	       (min (num num num))
	       (max (num num num))
	       )
	      (help "Theory for limits in  real functions."));;absval schon in natural



(th~defdef lim
	   (in limit)
	   (definition
	     (lam (f (num num))
		  (lam (a num)
		       (lam (L num)
			    (forall (lam (e num)
			    (exists (lam (d num)
			    (forall (lam (x num)
					 (implies (less 0 e)
						  (and (less 0 d)
						       (implies (and (greater (absval (minus x a)) 0)
								     (less (absval (minus x a)) d))
								(less (absval (minus (f x) L)) e))))))))))))))
	   (help "The prenex definition of the limit relation."))

(th~defdef cont
	   (in limit)
	   (definition
	     (lam (f (num num))
		  (lam (a num)
		       (forall (lam (e num)
				    (exists (lam (d num)
						 (forall (lam (x num)
							      (implies (less 0 e)
								       (and (less 0 d)
									    (implies (less (absval (minus x a)) d)
										     (less (absval (minus (f x) (f a))) e)))))))))))))
	   (help "The prenex definition of the continuous relation."))

(th~defdef fplus
	   (in limit)
	   (definition
	     (lam (f (num num))
		  (lam (g (num num))
		       (lam (x num)
			    (plus (f x)
				  (g x))))))
	   (help "The sum of unary functions."))

(th~defdef ftimes
	   (in limit)

	   (definition
	     (lam (f (num num))
		  (lam (g (num num))
		       (lam (x num)
			    (times (f x)
				   (g x))))))
	   (help "The product of unary functions."))


(th~defdef limseq
	   (in limit)
	   (definition
	     (lam (f (num num))
		  (lam (L num)
		       (forall (lam (e num)
		       (exists (lam (d num)
		       (forall (lam (x num)
				    (implies
				     (less 0 e)
				     (and (less 0 d)   ;d in Nat
					  (implies
					   (and
					    (less d x)
					    (less 0 x))   ;x in Nat
					   (less (absval
						  (minus (f x) l))
          		                         e))))))))))))))
		
(th~defdef sin
	   (in limit)
	   (definition
	     (lam (x num)
		  x))
	   (help "The sinus function."))

(th~defdef cos
	   (in limit)
	   (definition
	     (lam (x num)
		  x))
	   (help "The cosinus function."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(progn (format t "~%Loading the methods of the theory LIMIT ...~%")
;	 (th~input 'limit "methods"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

