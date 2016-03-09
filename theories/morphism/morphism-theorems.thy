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

 






(th~deftheorem ISOMORPHIC-IF-TWOHOMS
	       (in MORPHISM) 
	       (category THEOREM) 
	       
	       (conclusion (all-types aa bb
			    (forall (lam (set1 (o aa))
			     (forall (lam (op1 (aa aa aa))
			      (forall (lam (set2 (o bb))
			       (forall (lam (op2 (bb bb bb))
					    (equiv (isomorphic set1 op1 set2 op2)
						   (exists (lam (h (bb aa))
								(exists (lam (j (aa bb))
									     (and (and (and (forall-sort (lam (x aa)
													      (set2 (h x)))
													 set1)
											    (forall-sort (lam (x bb)
													      (set1 (j x)))
													 set2))
										       (and (homomorphism set1 op1 set2 op2 h)
											    (homomorphism set2 op2 set1 op1 j)))
										  (and (forall-sort (lam (x aa)
													 (= (j (h x)) x))
												    set1)
										       (forall-sort (lam (x bb)
													 (= (h (j x)) x))
												    set2)))))))))))))))))))
;; it would be sensible to have in the definition of homomorphism, that a homomorphism maps set1 into set (which is currently not the case)
;; If this would be the case, we could skip the first two entries of this theorem, since they would be contained in the definition of
;; homomorphism
	
