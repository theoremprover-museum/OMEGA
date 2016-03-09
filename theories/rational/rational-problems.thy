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



(th~defproblem 1-rational
	       (in rational)
	       (conclusion c (=
			    (plus (times (power -2/3 2) (minus 3 3/4)) (divide -7 5))
			    (divide 4 (times -10 (divide -3 -3))))))

(th~defproblem 2-rational
	       (in rational)
	       (conclusion c (less
			    (times 1/2 5)
			    (times 3/9 9))))

(th~defproblem 3-rational
	       (in rational)
	       (conclusion c (leq
			    (divide -5/3 -3/5)
			    (power 5/3 2))))

(th~defproblem 4-rational
	       (in rational)
	       (conclusion c (greater
			    (plus 1 (divide 2 10))
			    (minus 0 (divide 10 2)))))

(th~defproblem 5-rational
	       (in rational)
	       (conclusion c (geq
			    (divide 1/2 1/2)
			    (plus 1/2 1/2))))

(th~defproblem 6-rational
	       (in rational)
	       (conclusion c (in (divide -3 5) rat)))
