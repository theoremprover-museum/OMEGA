;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: THEORY -*-
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

(th~deftheory MULTI-SET
	      (uses list)
	      (help "Multi-sets and their operators."))

(th~deftype multi-set
	    (in multi-set)
	    (arguments 0)
	    (help "A simple type of multi-sets"))

(th~defconstant msnil
		(in multi-set)
		(type  multi-set)
		(help "The empty multi-set."))

(th~defaxiom last-base-ms
	       (in multi-set)
	       (formula
		(all-types aa
		 (forall (lam (a aa) (= (last (cons a msnil)) a)))))
	       (help "Last element of a multi-set."))

(th~defaxiom last-step-ms
	       (in multi-set)
	       (formula
		(all-types aa
		 (forall-sort (lam (L multi-set)
				  (forall (lam (a aa)
					       (= (last (cons a L)) (last L)))))
			     (lam (x multi-set)(not (= x msnil))))))		  
	       (help "Last element of a multi-set."))

(th~defaxiom length-base-ms
	       (in multi-set)
	       (formula
		(= (length msnil) 0))
	       (help "Length of the empty multi-set is 0."))

(th~defaxiom length-step-ms
	       (in multi-set)
	       (formula
		(all-types aa 
		(forall-sort (lam (L multi-set)
				  (forall (lam (a aa)
					       (= (length (cons a L)) (plus (length L) 1)))))
			     (lam (x multi-set)(not (= x msnil))))))							  
	       (help "Length of a multi-set (a L) = 1 + MSsize of L."))

(th~defconstant mssetminus
		(in multi-set)
		(type (all-types aa (multi-set aa multi-set)))
		(help "The setminus operator on multi-sets."))

(th~defaxiom mssetminus-base1
	     (in multi-set)
	     (formula
	      (all-types aa
			 (forall (lam (a aa)
				      (= (mssetminus msnil a) msnil)))))
	     (help "The taking an element out of the empty multi-set is still the empty multi-set."))

(th~defaxiom msssetminus-base2
	     (in multi-set)
	     (formula
	      (all-types aa
			 (forall (lam (a aa)
				      (forall (lam (M multi-set)
						   (= (mssetminus (cons a M) a) M)))))))
	     (help "({a} u M)\a = M, i.e. we remove the occurence of an element a in the multi-set."))
			 
(th~defaxiom msssetminus-step
	     (in multi-set)
	     (formula
	      (all-types aa
			 (forall (lam (a aa)
				 (forall (lam (b aa)
					  (forall (lam (M multi-set)
						  (implies (not (= a b))
							   (= (mssetminus (cons b M) a) (mssetminus M a)))))))))))
	     (help "({b} u M)\a = M\a if (a!=b)."))
			 


(th~defaxiom =multi-set
            (in multi-set)
            (formula
             (all-types aa ll
                        (forall (lam (S multi-set)
                                     (forall (lam (T multi-set)
                                                  (equiv (= S T)
							 (= (rest S) (mssetminus T (first S))))))))))
	    (help "Two multisets S and T are equal iff the rest of S (S with its first element removed) is equal to T \ (first S)."))
