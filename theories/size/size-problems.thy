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


(th~defproblem total-size-coefficient
  (in economy)
  (conclusion (forall (lam (sz num)
	       (forall (lam (u num)
		    (implies (and (sizeunit u) (compatible-sizes sz u))
			     (defined (size-coefficient sz u))))))))
  (help "The unit of a size is always defined."))


(th~defproblem pplus-size-function
   (in economy)
   (conclusion conc
	       (forall (lam (F (num num))
	        (forall (lam (G (num num))
	         (forall (lam (u num)
	          (forall (lam (v num)
		   (= (p-plus-r1 (size-function F u v)
			     (size-function G u v))
		      (size-function (p-plus-r1 F G) u v)))))))))))
   (help "The pointwise sum of two size fuctions is the size function
          given by the pointwise sum."))



(th~defproblem composition-size-function
   (in economy)
   (conclusion conc
	       (forall (lam (F (num num))
	        (forall (lam (G (num num))
	         (forall (lam (u num)
	          (forall (lam (v num)
	           (forall (lam (w num)
		   (= (compose-functions (size-function F w v)
			     (size-function G v u))
		      (size-function (compose-functions F G) w u)))))))))))))
   (help "The composition of two size fuctions is the size function
          given by the compositions."))

;(th~defproblem unique-num-size
;  (in economy)
;  (conclusion conc (forall (lam (sz num) (defined (size-num sz)))))
;  (help "The numerical part of a size is always defined."))

(th~defproblem real-times-size 
  (in size)
  (conclusion (forall (lam (a num)
                     (forall (lam (b num)
                       (forall (lam (u num)
                         (implies (and (sizeunit u) (and (real a) (real b)))
                           (= (times a (times b u)) (times (times a b) u))))))))))
  (help "S-multiplication of sizes"))

#|
(th~defproblem inverse-application 
  (in size)
  (conclusion (forall (lam (u num)
                (implies (sizeunit u) (= one (times u (inverse u))))))))     
  (help "multiplication by the inverse"))
|#
