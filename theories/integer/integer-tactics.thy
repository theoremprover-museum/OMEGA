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
(eval-when (load compile eval)
  (unless (com~find-category 'integer)
    (com~defcategory integer
		     (help "Tactics of the theory  integer ."))))


;;;;;;Tactic for primes

(infer~deftactic prime
		 (outline-mappings (((existent) prime-a)))
		 (expansion-function int=expand-prime)
		 (help "Justifies (prime p)."))

(tac~deftactic prime-a prime
	       (in integer)
	       (conclusions L1)
	       (sideconditions (int=check-prime (:formula L1)))
	       (description "Justifies (prime p)."))


(defun int=check-prime (form)
  (when (and (data~appl-p form)
	     (data~equal (data~appl-function form) (env~lookup-object 'prime (pds~environment omega*current-proof-plan))))
    (let ((number (keim~name (car (data~appl-arguments form)))))
      (labels ((div-smaller (num)
			    (or (= num 1)
				(and (not (integerp (/ number num)))
				     (div-smaller (1- num))))))
	(and (integerp number) (> number 1) (div-smaller (1- number)))))))
    
(com~defcommand prime
  (argnames line1)
  (argtypes ndline)
  (arghelps "A line containing (prime p)")
  (function int=prime)
  (frag-cats tactics integer)
  (defaults)
  (log-p t)
  (help "Justifies (prime p)."))

(defun int=prime (l1)
  (infer~compute-outline 'prime (list l1) nil))

(defun int=expand-prime (outline))







;;;;;;Tactic for not=int

(infer~deftactic not=int
		 (outline-mappings (((existent) not=int-a)))
		 (expansion-function int=expand-not=int)
		 (help "Justifies (not (= a b)) for two integers a,b."))

(tac~deftactic not=int-a not=int
	       (in integer)
	       (conclusions L1)
	       (sideconditions (int=check-not=ab (:formula L1)))
	       (description "Justifies (not (= a b)) for two integers a,b."))


(defun int=check-not=ab (formula)
  (if (logic~negation-p formula)
      (let* ((arg (first (data~appl-arguments formula))))
	(if (and (data~appl-p arg)
		 (keim~equal (data~appl-function arg) (data~schema-range (env~lookup-object '= (th~env 'base)))))
	    (let* ((a (first (data~appl-arguments arg)))
		   (b (second (data~appl-arguments arg))))
	      (if (and (term~constant-p a)
		       (term~constant-p b)
		       (integerp (keim~name a))
		       (integerp (keim~name b))
		       (null (= (keim~name a) (keim~name b))))
		  't
		nil))
	  nil))
    nil))
	     

(com~defcommand not=int
  (argnames line1)
  (argtypes ndline)
  (arghelps "A line containing (not (= a b)).")
  (function int=not=int)
  (frag-cats tactics integer)
  (defaults)
  (log-p t)
  (help "Justifies (not (= a b)) for two integers a,b."))

(defun int=not=int (l1)
  (infer~compute-outline 'not=int (list l1) nil))

(defun int=expand-not=int (outline)
  (let* ((line (first outline)))
    ))
    

