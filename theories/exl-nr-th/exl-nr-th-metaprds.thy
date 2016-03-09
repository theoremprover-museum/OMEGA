;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
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


(meth~defcond eln-abstractable  (args tmapp)
  (declare (edited  "03-NOV-1999")
	   (authors Gebhard)
	   (input   )
	   (effect  )
	   (value   ))
  ;; Bedingung fuer das Abstrahieren von einem Teilziel
  (let* ((opform (first args))
	 (linelab (second args))
	 (eln-abstr-cond1 (box-in-S-case opform))
	 (eln-abstr-cond2 (every-x-unit opform))
	 (eln-abstr-cond3 (exists-units opform))
	 (eln-abstr-cond4 (resres-rule opform))
	 (eln-abstr-cond5 (derivation-correct opform))
	 (eln-abstr-cond6 (eq-append opform))
	 (eln-abstr-cond7 (poslist2info opform))
	 (eln-abstr-cond8 (that-expresions opform))
	 (eln-abstr-cond9 (=reflex opform))
	 )
    (if (or eln-abstr-cond1 eln-abstr-cond2 eln-abstr-cond3
	    eln-abstr-cond4 eln-abstr-cond5 eln-abstr-cond6
	    eln-abstr-cond7 eln-abstr-cond8 eln-abstr-cond9)
	tmapp
      (meth~tm-new-value tmapp nil))))

