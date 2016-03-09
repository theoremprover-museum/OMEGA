;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                         ;;
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


(th~deftheorem submodule-property
	       (in module)
	       (conclusion
		(all-types aa bb
		 (forall (lam (M (o bb))
		  (forall (lam (madd (bb bb bb))
		   (forall (lam (R (o aa))
		    (forall (lam (radd (aa aa aa))
		     (forall (lam (rmul (aa aa aa))
		      (forall (lam (rmmul (bb bb aa))
		       (forall (lam (A (o bb))
			 (implies (and (and (module M madd R radd rmul rmmul)
					    (subset A M))
				       (and (closed-under A madd)
					    (and (in (group-unit M madd) A)
						 (and (forall-sort (lam (m1 bb)
						       (forall-sort (lam (m2 bb)
							(in (madd m1 ((group-inverse M madd) m2)) A))
								    A))
								   A)
						      (forall-sort (lam (r1 aa)
						       (forall-sort (lam (m1 bb)
							(in (rmmul r1 m1) A))
								    A))
								   R)))))
				  (submodule A M madd R radd rmul rmmul)))))))))))))))))))
