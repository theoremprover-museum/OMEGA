;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package "KEIM")

(mod~defmod hou-clash :uses (uni-main) 
	    :documentation "Modifications for ho-clash unification."
	    :exports (uni+clash
                      uni~clash-push
                      uni~clash-pop
		      uni~clash
                      )
	    )

;;
;; Higher Order Clash unification
;; 

(eval-when (load compile eval) 
  (defclass uni+clash (uni+hou)
    ((clash :accessor uni~clash :initarg :clash
            :initform nil))
    (:documentation 
     "Uni+clash are HOU problems where unification does not fail,
      but ptoduces clash-pairs.")))


(defmethod keim~copy ((object uni+clash)
                      &key (explode :all-classes) (share :all-classes)
                      preserve downto)
  (let ((newobj (uni~create object
                            :terms (uni~terms object))))
    (setf (uni~flex-rigid newobj) (uni~flex-rigid object))
    (setf (uni~flex-flex newobj) (uni~flex-flex object))
    (setf (uni~substitution newobj) (uni~substitution object))
    (setf (uni~clash newobj) (uni~clash object))
    (setf (uni~bill newobj) (uni~bill object))
    (setf (uni~link newobj) (uni~link object))
    newobj))


(defgeneric uni~clash-push (termpair uni))

(defmethod uni~clash-push ((termpair cons)(uni uni+clash))
  (declare (edited  "11-DEC-1996")
	   (authors Konrad)
	   (input   "a termpair (cons) and a uni+clash structure.")
	   (effect  "push termpair into termlist of UNI.")
	   (value   "the new termlist."))
  (setf (uni~clash uni)
        (cons termpair (uni~clash uni))))

(defgeneric uni~clash-pop (uni))

(defmethod uni~clash-pop ((uni uni+clash))
  (declare (edited  "11-DEC-1996")
	   (authors Konrad)
	   (input   "a uni+clash structure.")
	   (effect  "pops first element from clashtack.")
	   (value   "the first termpair."))
  (let ((pop (first (uni~clash uni))))
    (setf (uni~clash uni)
          (rest (uni~clash uni)))
    pop))

(defmethod print-object ((object uni+clash) (stream stream))
  (format stream "~a" (list "CLASH"
                            "terms:" (uni~terms object)
                            "subst:" (uni~substitution object)
                            "flex-flex:" (uni~flex-flex object)
                            "flex-rigid:" (uni~flex-rigid object)
                            "clash:" (uni~clash object)
			    "bill:" (uni~bill object))))

	    
(defmethod hou~simplify-aux ((term1 term+constant) (term2 term+constant)
                             unsolved flex-stack (uni uni+clash))
  (declare (edited  "12-Dec-1996")
	   (authors KK)
	   (input   "two terms a list of unsolved termpairs and a list"
		    "of flex-stack-pairs")
	   (effect  "stores clashed pairs of constants into the
                     unification problem.")
	   (value   "always calls next simplification"))
  (if (hou~trivial-p term1 term2 uni)
      (hou~simplify-next unsolved flex-stack uni) ;; pair does not clash
    ;; else
    (progn (uni~clash-push (list term1 term2) uni)
           (hou~simplify-next unsolved flex-stack uni))))


(defmethod hou~simplify-aux ((term1 term+appl) (term2 term+appl)
                             unsolved flex-stack (uni uni+clash))
  (let ((decompose (hou~decompose term1 term2 uni)))
    (if decompose
        (if (equal decompose :flex)
            ;; (flex-something)
            (hou~simplify-next unsolved
                               (cons (list term1 term2) flex-stack) uni)
          (hou~simplify-next (append decompose unsolved)
                             flex-stack uni))
      (progn (uni~clash-push (list term1 term2) uni)
             (hou~simplify-next unsolved flex-stack uni)))))


