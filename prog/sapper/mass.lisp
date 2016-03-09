;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: KEIM -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
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

;;; MASS
;;;
;;; My Algebraic Support System
(in-package :omega)

(mod~defmod MASS
	    :uses ()
	    :documentation "Some algebraic algorithms."
	    :exports (
	    mass~eql
	    mass~plus
	    mass~minus
	    mass~times
	    mass~div
            mass~sqrt
	    mass~power
	    mass~inverse
	    mass~zero
	    mass~one
	    mass~read
	    mass~create
	    mass~rebuild
	    mass~prove-eql)
	    )


;; KEIM objects I use:

(defconstant mass*plus           :plus)
(defconstant mass*mult           :times)
(defconstant mass*sub            :minus) 
(defconstant mass*div            :div)
(defconstant mass*power          :power)
(defconstant mass*sqrt           :sqrt)
(defconstant mass*expt           :expt)
(defconstant mass*mod            :mod)
(defconstant mass*equality       :=)
(defconstant mass*num            :num)
(defconstant mass*pplus          :p-plus-r1)
(defconstant mass*stimes         :s-times-r1)
(defconstant mass*pderiv         :p-deriv-r1)
(defconstant mass*Empty          :EmptyList)
(defconstant mass*unknown        :unknown)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The abstract CAS for the simplification of terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ca~defsystem :MASS
	      (help "Polynomials and nothing but polynomials")
	      (translations
	       (plus  (mass~plus  (:mass-term :mass-term2 :mass-term1)))
	       (times (mass~times (:mass-term :mass-term2 :mass-term1)))
	       (minus (mass~minus (:mass-term :mass-term2 :mass-term1)))
	       (power (mass~power (:mass-term :mass-term2 :mass-term1)))
               (div   (mass~div   (:mass-term :mass-term2 :mass-term1)))
               (sqrt  (mass~sqrt  (:mass-term :mass-term1)))
	       (mod   (mass~mod   (:mass-term :mass-term2 :mass-term1)))
	       (=     (mass~equal (:mass-term :mass-term2 :mass-term1))))
	      (call eval))

(defmethod ca~build-object (term (object (eql :mass-term1)) (system (eql :mass)))
  (ca~output-method '((:forward . init-mass-f) (:backward . init-mass-b)))
  (mass~read term (pos~list-position '(1))))

(defmethod ca~build-object (term (object (eql :mass-term2)) (system (eql :mass)))
  (mass~read term (pos~list-position '(2))))

(defmethod ca~build-object (term (object (eql :mass-term3)) (system (eql :mass)))
  (mass~read term (pos~list-position '(3))))
  

(defmethod ca~rebuild-object (term (object (eql :mass-term)) (system (eql :mass)))
  (mass~rebuild term))


 
