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

(th~defproblem boolos-curious-inference
  (in boolos)
  (assumption a1 (forall (lam (n i) (= (f n one) (s one)))))
  (assumption a2 
    (forall (lam (x i) (= (f one (s x)) (s (s (f one x)))))))
  (assumption a3 
    (forall (lam (n i) 
      (forall (lam (x i) (= (f (s n) (s x)) (f n (f (s n) x))))))))
  (assumption a4 (D one))
  (assumption a5 (forall (lam (x i) (implies (D x) (D (s x))))))
  (conclusion conc (D (f (s (s (s (s one)))) (s (s (s (s one))))))))


;;;; The following Lemmata are prposed by Boolos

;; lemma conc (N one)
;; lemma conc (forall (lam (y i) (implies (N y) (N (s y)))))
;; lemma conc (N (s (s (s (s one)))))
;; lemma conc (E one)
;; lemma conc (forall (lam (y i) (implies (E y) (E (s y)))))
;; lemma conc (E (s one))


;; lemma conc (forall (lam (nn i) (implies (N nn) (forall (lam (x i) (implies (N x) (E (f nn x))))))))
;; -> lemma ?? (M one)
;; -> -> lemma ?? (forall (lam (x i) (implies (N x) (Q x)))) 
;; -> -> -> lemma ?? (Q one)
;; -> -> -> lemma ?? (forall (lam (x i) (implies (Q x) (Q (s x)))))
;; -> lemma ?? (forall (lam (y i) (implies (M y) (M (s y)))))
;; -> -> lemma ?? (forall (lam (x i) (implies (N x) (P x)))) 
;; -> -> -> lemma ?? (P one)
;; -> -> -> lemma ?? (forall (lam (x i) (implies (P x) (P (s x)))))
