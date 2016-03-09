;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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

 (in-package :omega)


(meth~defcond mnot-empty-pos (args tmapp)
  (declare (edited  "07-FEB-2000")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
	      (let* ((position (first args))
		     (empty (or (null position)
				(null (pos~p position))
				(pos~empty-p position))))
		
		(if empty
		    (meth~mapp-new-constraint tmapp nil)
		  tmapp)))

;; (meth~deffun mfocussednewprln (hyps formula ass-node position)
;;   (declare (edited  "07-FEB-2000")
;; 	   (authors Ameier)
;; 	   (input   )
;; 	   (effect  )
;; 	   (value   ))
;; 	     (progn ;; (format t "~%ICH BINS")
;; 	       (let* ((label (pds~new-node-name))
;; 		      (just (pdsj~open-just-create))
;; 		      (new-node (pdsn~create label hyps formula just))
;; 		      )
;; 		 (black~add-new-blackboard-object! 'focus 
;; 						   (list (keim~name new-node)
;; 							 (keim~name ass-node)
;; 							 position)
;; 						   'focus
;; 						   sod*solution-blackboard)
;;       	 
;; 		 ;; (format t "~%HIER BIN ICH FERTIG")
;;		 
;; 		 ;;(list new-node))))
;; 		 new-node)))

(meth~deffun mfocussednewprln (hyps formula ass-node position)
  (declare (edited  "07-FEB-2000")
	   (authors Ameier)
	   (input   )
	   (effect  )
	   (value   ))
	     (progn ;; (format t "~%ICH BINS")
	       (let* ((label (pds~new-node-name))
		      (just (pdsj~open-just-create))
		      (new-node (pdsn~create label hyps formula just))
		      )
		 new-node)))


(defun crihelp=initial-strategy-p ()
  (let* ((current-steps (roc~pplanner-steps pplan*roc-state-description)))
    (if (null current-steps)
	't
      nil)))
