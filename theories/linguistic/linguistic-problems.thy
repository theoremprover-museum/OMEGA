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


(th~defproblem ap-all
	       (in linguistic)
	       (conclusion c	   (antipersistency all)))

(th~defproblem p-a
	       (in linguistic)
	       (conclusion c	   (persistency a)))

(th~defproblem dm-no
	       (in linguistic)
	       (conclusion c	   (downwardmonotonic no)))

(th~defproblem um-all
	       (in linguistic)
	       (conclusion c	   (upwardmonotonic all)))

(th~defproblem yellow
	       (in linguistic)
	       (constants (isyellow (o i))
			  (submarine i))
	       (assumption a1     (isyellow submarine))
	       (conclusion c	   (atleast one-thing isyellow)))

(th~defproblem drink-man
	       (in linguistic)
               (constants (Man (o i))
			  (Drink (o i)))
	       (conclusion c (not (and (most man drink)
				       (no man drink)))))
	     
(th~defproblem british-english
	       (in linguistic)
       	       (constants (english (o i))
			  (british (o i)))
               (assumption a1 (forall (lam (people i) (implies (english people)
							       (british people)))))
	       (conclusion c (implies (atmost two-things british )
				      (atmost two-things english ))))
				    
(th~defproblem british-english3
	       (in linguistic)
       	       (constants (english (o i))
			  (british (o i)))
               (assumption a1 (forall (lam (people i) (implies (english people)
							       (british people)))))
	       (conclusion c (implies (atmost three-things british )
				      (atmost three-things english ))))

(th~defproblem british-english4
	       (in linguistic)
       	       (constants (english (o i))
			  (british (o i)))
               (assumption a1 (forall (lam (people i) (implies (english people)
							       (british people)))))
	       (conclusion c (implies (atmost four-things british )
				      (atmost four-things english ))))


