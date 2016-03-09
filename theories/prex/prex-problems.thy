;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: THEORY -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 2000 by Armin Fiedler and AG Siekmann,                   ;;
;;   Fachbereich Informatik, Universitaet des Saarlandes,                   ;;
;;   Saarbruecken, Germany.                                                 ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, see the P.rex home page at:        ;;
;;     http://www.ags.uni-sb.de/~prex                                       ;;
;;   or write to:                                                           ;;
;;     P.rex Project                                                        ;;
;;     AG Siekmann/FR Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 151150                                                      ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: prex@ags.uni-sb.de                                    ;;
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

(th~defproblem prex-test
	 (in prex)
	 (constants
	  (a i)
	  (U (o i))
	  (V (o i))
	  )
	 (assumption ass
		     (or (in a U) (in a V)))
	 (assumption union
		     (forall (lam (U (o i))
				  (forall (lam (V (o i))
					       (forall (lam (x i)
							    (equiv (in x (union U V))
								   (or (in x U) (in x V))))))))))
	 (conclusion th
		     (in a (union U V))))
