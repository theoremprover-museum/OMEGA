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

(in-package :omega)



(mod~defmod PVS
            :uses (atpprb atptop hocnf just keim node omega otter p2f res sys)
            :documentation "Calling PVS in Omega."
            :exports (
                      pvs~prove
                      ))


(defun pvs~prove (Formula Tactic Library)
  (let* ((service (serv~enter "PVS"))
         )
    (when service
      (let* ((om-formula (format nil "'OMOBJ'(~A)" (omcont~string formula)))
             ;;              (message (format nil "prove(~A tactic: '~A' library: '~A')"
             ;;                               om-formula
             ;;                               Tactic
             ;;                               Library))
             (message (format nil "provects(~A)"
                              om-formula))
             )
        (format t "~%message: ~S" message)
        (let ((result (serv~apply "PVS" message)))
          (format t "~%PVS result: ~S~%" result)
          (serv~leave "PVS")
          result)
        ))))

        
        
