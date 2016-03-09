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

 


(th~deftheorem equal-reflexivity 
	       (in POST) 
	       (conclusion
		(all-types aa (FORALL-SORT (lam (X aa) (EQUAL X X)) DEFINED)))
	       (help "Reflexivity for strict =."))


(th~deftheorem equal-substitution
	       (in POST) 
	       (conclusion
		(all-types bb
			   (FORALL (lam (form (o bb))  
			   (FORALL-SORT (lam (subterm1 bb)
			   (FORALL-SORT (lam (subterm2 bb)
						 (implies (and (EQUAL subterm2 subterm1)
							       (form subterm1))
							  (form subterm2))) defined )) defined )))))
	       (help "Substitution for strict =."))

(th~deftheorem equal-symmetry
	       (in POST) 
	       (conclusion
		(all-types bb
			   (FORALL-SORT (lam (term1 bb)
			   (FORALL-SORT (lam (term2 bb)			  
					(implies (equal term1 term2)
						 (equal term2 term1))) defined)) defined)))
	       (help "Symmetry for strict =."))
