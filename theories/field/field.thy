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

(th~deftheory field
              (uses ring)
	      (help "The theory of fields"))


(th~defdef field
	   (in field)
	   (type-variables aa)
           (definition
            (lam (S (o aa))
		 (lam (add (aa aa aa))
		      (lam (mul (aa aa aa))
			   (and (abelian-group S add)
				(group (setminus S (singleton (group-unit S add))) mul))))))
	   (help "The definition of a field."))

(th~defdef field-oneover
	   (in field)
	   (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (struct-inv (struct-mul-sgroup S))))
	   (help "The multiplicative inversion for fields."))


(th~defdef field-div
           (in field)
           (type-variables aa)
           (definition
             (lam (S (struct aa))
                  (lam (x aa)
                       (lam (y aa)
                            (ring-times S x (field-oneover S y))))))
           (help "The division function for fields."))

