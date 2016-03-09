;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     PROVERB Project                                                      ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: proverb@cs.uni-sb.de                                  ;;
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


(mod~defmod ALTAC 
            :uses (alexp alift infer mod tacl)
            :documentation "Assertion level tactics."
            :exports (
                      
                      ))
#+old(mod~defmod ALTAC 
            :uses (alexp alift infer tacl)
            :documentation "Assertion Level Tactics."
            :exports (
                      
                      ))


;#########################################
;##                                     ##
;##             Taktiken                ##
;##                                     ##
;#########################################


(infer~defbbox assertion
	       (outline-function altac=outline)
	       (test-function)
	       (expansion-function altac=expand-function)
	       (parameter-types)
	       (help "Assertion level."))

(defun altac=outline (outline para)
  (declare (edited  "27-JUN-1997")
	   (authors Afiedler)
	   (input   "Outline is a list of the conclusion, the assertion to be applied, and"
		    "the premises. PARA should always be NIL.")
	   (effect  "Applies an assertion to outline.")
	   (value   "OUTLINE.")
	   (ignore para))
  (alift~apply-assertion-tactic (cddr outline) (cadr outline) (car outline))
  outline)

(defun altac=expand-function (outline params)
  (declare (edited  "30-JUN-1997")
	   (authors Afiedler)
	   (input   "An outline and nil.")
	   (effect  "Expands an assertion level node.")
	   (value   "Undefined.")
	   (ignore params))
  (omega::tacl~init outline)
  (alexp~expand (car outline))
  (omega::tacl~end))
