;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: KEIM -*-
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


;;(infer~defmethod "totmin-rolle"
;;                 (outline-mappings (((existent
;;                                      nonexistent
;;                                      nonexistent
;;                                      nonexistent
;;                                      nonexistent) totmin-rolle)))
;;                 (help "Finding a total minimum using Rolle's theorem"))
;;
;;
;;(meth~defmethod totmin-rolle totmin-rolle
;;                (in calculus)
;;                (rating 1)
;;                (declarations
;;                 (sorted-meta-variables
;;                  (F (num num)) (min num term) (I (o num))
;;                  (a num) (b num)))
;;                (premises  (+ p1))
;;                (application-condition
;;                 (mand (mand (verify-with-cas (polynomial F))
;;                             (verify-with-cas (= (degree F) 2)))
;;                       (mand (mbind min (compute-with-cas (local-min sf-fun F)))
;;                             (mand (mand (verify-with-cas (greater (F a) (F min)))
;;                                         (verify-with-cas (greater (F b) (F min))))))))
;;                (expansion-constraint
;;                 (mbind rolle (th-assertion "Rolles-thm")))
;;                (conclusions (- conc))
;;                (decl-content
;;                 (p1 h (closed-interval-with-bounds a b I))
;;                 (c1 h (polynomial F) (call-cas () ()))
;;                 (c2 h (= (degree F two)) (call-cas () ()))
;;                 (c3 () (= (derivative F min) zero) (call-cas () ()))
;;                 (c4 () (greater (nderivative two F min) zero) (call-cas () ()))
;;                 (c5 () (greater (F a) (F min)) ("call-cas" () ()))
;;                 (c6 () (greater (F b) (F min)) ("call-cas" () ()))
;;                 (l1 h  (local-minimum F min) ("ass" diff (c3 c4)))
;;                 (conc h (total-minimum min F I) ("ass" rolle (l1 c5 c6 c1 c2 p1)))
;;                 )
;;                (proc-content schema-interpreter)
;;                (remark "test")
;;                )
;;
;;
;;
;;
;;
