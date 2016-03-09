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



(mod~defmod LCLAM
            :uses (atpprb atptop hocnf just keim node omega otter p2f res sys)
            :documentation "Calling LCLAM in Omega."
            :exports (
                      lclam~plan-problem
                      ))


(defun lclam~plan-problem (NDLine Timeout)
  (let* ((service (serv~enter "LCLAM" :mode "xterm" ))
         )
    (when service
      (let* ((formula (node~formula NDLine))
             (om-formula (format nil "'OMOBJ'(~A)" (omcont~string formula)
                                 )
                         )
             (message (format nil "planProblem(~A timeout: ~A)"
                              om-formula
                              Timeout))
             )
        (format t "~& message: ~S" message)
        (let ((result-str (serv~apply "LCLAM" message :timeout Timeout)))
          (format t "~%~%LCLAM result: ~S~%" result-str)
          
          (serv~leave "LCLAM") ;; need that because we need a fresh lclam next time :-(
          (when result-str
            (let* ((result (read-from-string result-str))
                   (asrd (format t "~& result: ~S" result))
                   (alist (when (and (listp result) (listp (cdr result)))
                            (car (cdr result))))
                   (asrd (format t "~& alist: ~S" alist))
                   (state-pair (assoc 'state alist))
                   (state (when state-pair (cdr state-pair)))
                   )
              (format t "~& state: ~S" state)
              (when (and state (string-equal state "planned"))
                (let* ((new-just (pdsj~closed-just-create (infer~find-method 'lclam)
                                                          nil
                                                          nil
                                                          "untested"))
                       )
                  (setf (pdsj~control new-just) (pdsj~control (node~justification NDLine)))
                  (atp=insert-just! NDLine new-just)
                  (list state))
                ))))))))

(defun lclam~ripple (Goal WaveRulesNames Timeout)
  (let* ((service (serv~enter "LCLAM" :mode "xterm" ))
         )
    (when service
      (let* ((sequent-omdoc (omcont~string Goal))
             (theory-name (keim~name (prob~proof-theory omega*current-proof-plan)))
             (wave-rules-omdoc-list
              (omcont~string
               (mapcar #'(lambda (rulename)
                           (th~find-problem&assumption rulename theory-name)
                           )
                       WaveRulesNames)))
             (wave-rules-omdoc (format nil "'omdoc'(items: ~A)"
                                       wave-rules-omdoc-list))
             (set-message (format nil "setWaveRules(~A)" wave-rules-omdoc))
             
             (goal-omdoc (omcont~string goal))
             (ripple-message (format nil "ripple(~A)"
                                     goal-omdoc
                                     Timeout))
             
             )
;        (format t "~S" wave-rules-omdoc)
;        (format t "~& message: ~S" set-message)
;        (format t "~& message: ~S" ripple-message)
        (let ((result1 (serv~apply "LCLAM" set-message :timeout Timeout)))
          (format t "~%~%setWaveRules result: ~S~%" result1)
          (let ((result-str (serv~apply "LCLAM" ripple-message)))
            (serv~leave "LCLAM") ;; need that because we need a fresh lclam next time :-(
            (when result-str
              (let* ((result (read-from-string result-str))
                     (asrdf (format t "~& ripple result: ~S" result))
                     (alist (when (and (listp result) (listp (cdr result)))
                              (car (cdr result))))
                     (asrd (format t "~& alist: ~S" alist))
                     (state-pair (assoc 'state alist))
                     (state (when state-pair (cdr state-pair)))
                     )
                (format t "~& state: ~S" state)
                (when (and state (string-equal state "planned"))
                  (list nil nil)
                  )))))))))


(defun lclam~compute-justification-content (outline parameters)
  nil
  )