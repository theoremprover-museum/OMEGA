;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                   Module                                 ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)



(mod~defmod PL2P 
            :uses (data node p2pl prob)
            :documentation "Translating propositionl problems back to higher-order."
            :exports (
                      
                      pl2p~translate
                      ))






(defun pl2p~translate (pds)
  (declare (edited  "21-APR-1998")
	   (authors Ameier)
	   (input   "A pds.")
	   (effect  "The formulas of the pds are translated back into higher order logic formulas."
		    "Every occurrence of a constant of p2pl*codomain is replaced by a copy of the"
		    "according p2pl*domain term.")
	   (value   "The chenged pds."))
  (omega~message "Translating from propositional- to higher-order logic.")
  (mapcar #'(lambda (node)
	      (setf (node~formula node)
		    (data~replace-structs (node~formula node)
					  p2pl*codomain
					  p2pl*domain
					  :downto '(data+primitive)
					  :replacers-downto '(data+primitive)))
	      (setf (pdsj~parameters (node~justification node))
		    (mapcar #'(lambda (param)
				(if (term~p param)
				    (data~replace-structs param
							  p2pl*codomain
							  p2pl*domain
							  :downto '(data+primitive)
							  :replacers-downto '(data+primitive))
				  param))
			    (pdsj~parameters (node~justification node)))))
	  
	  (prob~proof-steps pds))
  
  pds)

  
