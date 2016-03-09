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

(th~defproblem wiwi-test
  (in economy)
  (assumption price-cbm (sizeunit-factor 2 dm cbm))
  (assumption price-kwh (sizeunit-factor 0.5 dm kwh))
  (conclusion 
		(opt (p-plus-r1 (size-function
			     (lam (z num)
				  (plus (times 0.5 (power z 2)) 3))
			     kwh      ;;; was cbm
			     prod)
			    (size-function
			     (lam (z num)
				  (plus (times 4 (power z 2))
					(plus (times -24 z) 6)))
			     kwh
			     prod))
		    (closed-interval-bounds 1 7)))
  (help "An optimization problem from an SB economy exam."))

(th~defproblem wiwi-exam
  (in economy)
  (assumption price-cbm (sizeunit-factor 2 dm cbm))
  (assumption price-kwh (sizeunit-factor 0.5 dm kwh))
  (conclusion 
		(opt (p-plus-r1 (size-function
			     (lam (z num)
				  (plus (times 0.5 (power z 2))
					(times 3 (power z 0))))
			     cbm
			     prod)
			    (size-function
			     (lam (z num)
				  (plus (times 4 (power z 2))
					(plus (times -24 (power z 1))
					      (times 6 (power Z 0)))))
			     kwh
			     prod))
		    (size-set (closed-interval-bounds 1 7) DM)))
  (help "An optimization problem from an Saarbruecken economy exam."))

(th~defproblem wiwi-exam2
  (in economy)
  (assumption price-cbm (sizeunit-factor 2 dm cbm))
  (assumption price-kwh (sizeunit-factor 0.5 dm kwh))
  (assumption price-oil (sizeunit-factor 1 dm liter))
  (conclusion 
		(opt (p-plus-r1 (size-function
			     (lam (z num)
				  (plus (times 0.5 (power z 2))
					(times 3 (power z 0))))
			     cbm
			     prod)
			    (p-plus-r1 (size-function
					(lam (z num)
					     (plus (times 4 (power z 2))
						   (plus (times -24 (power z 1))
							 (times 6 (power Z 0)))))
					kwh
					prod)
				       (size-function
					(lam (z num)
					     (plus (times 1 (power z 2))
						   (plus (times -6 (power z 1))
							 (times 2 (power Z 0)))))
					liter
					prod)))
		    (size-set (closed-interval-bounds 1 7) DM)))
  (help "An optimization problem from an Saarbruecken economy exam."))

(th~defproblem opt-test
  (in economy)
  (conclusion 
   (opt (size-function
	 (lam (z num)
	      (plus (times 3 (power z 2))
		    (plus (times -12 (power z 1))
			  (times 9 (power z 0)))))
	 DM
	 prod)
	(size-set (closed-interval-bounds 1 7) DM)))
  (help "An optimization problem from an SB economy exam."))
