; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: keim -*-
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

(in-package "OMEGA")

;; sign/(sign)*

(defconstant sgn*plus "+")
(defconstant sgn*minus "-")
(defconstant sgn*poly "*")

;; Legal sign lists are:
;; - ("+")
;; - ("-")
;; - ("*" "*" "*") (n-times)
;; Illegal sign list:
;; - ("+" "-")

;; Legal sign tuples are:
;; - ()
;; - ("+" "-")
;; Illegal are lists containing poly signs

(defun sgn~plus-p (sgn)
  (eq sgn sgn*plus))

(defun sgn~minus-p (sgn)
  (eq sgn sgn*minus))

(defun sgn~poly-p (sgn)
  (eq sgn sgn*poly))

(defgeneric sgn~negate (sgns)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A single sign or a list of signs.")
	   (effect  "None.")
	   (value   "The negated sign or a list of negated signs (The negation of a poly sign is a poly sign)."))
  (:method ((sgns list))
	   (mapcar 'sgn~negate sgns))
  (:method (sgn)
	   (cond ((sgn~plus-p sgn) sgn*minus)
		 ((sgn~minus-p sgn) sgn*plus)
		 ((sgn~poly-p sgn) sgn*poly)
		 (t (omega~error "Sign is no valid constant: ~A" sgn)))))

(defun sgn~add-poly-sign (sgns)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A sign list.")
	   (effect  "None.")
	   (value   "A poly sign added to the list following the rules: +* -> *, -* -> *, *** -> ****"))
  (cond ((sgn~plus-p (first sgns)) (list sgn*poly))
	((sgn~minus-p (first sgns)) (list sgn*poly))
	((every #'(lambda (sgn) (sgn~poly-p sgn)) sgns) (cons sgn*poly sgns))
	(t (omega~error "Signs is no valid list: ~A" sgns))))

(defun sgn~positive-p (sgns)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A sign list.")
	   (effect  "None.")
	   (value   "True iff the list consist of plus or poly signs."))
  (or (sgn~plus-p (first sgns)) (sgn~poly-p (first sgns))))
  
(defun sgn~negative-p (sgns)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A sign list.")
	   (effect  "None.")
	   (value   "True iff the list consists of minus or poly signs."))
  (or (sgn~minus-p (first sgns)) (sgn~poly-p (first sgns))))



(defclass sgn+tuple ()
  ((tuple             :initarg :tuple
		      :initform nil
		      :accessor sgn=tuple-list)))

(defun sgn~tuple-p (obj)
  (typep obj 'sgn+tuple))

(defmethod print-object ((sgn-tuple sgn+tuple) stream)
  (let ((sgns (sgn=tuple-list sgn-tuple)))
    (if sgns
	(format stream "~A" (sgn=tuple-list sgn-tuple))
      (format stream "no signs"))))

(defmethod keim~equal ((sgn-tuple1 sgn+tuple) (sgn-tuple2 sgn+tuple))
  (keim~equal (sgn=tuple-list sgn-tuple1) (sgn=tuple-list sgn-tuple2)))

(defun sgn~tuple-create (sgns)
  (make-instance 'sgn+tuple :tuple sgns))

(defun sgn~tuple-first (sgn-tuple)
  (first (sgn=tuple-list sgn-tuple)))

(defun sgn~tuple-rest (sgn-tuple)
  (sgn~tuple-create (rest (sgn=tuple-list sgn-tuple))))

(env~enter 'signs       (ssterm~ssort-create 'signs)         ssterm*ssort-env)

(defmethod meth=syntactic-sort-p ((sgn-tuple sgn+tuple) (ssort ssterm+syntactic-sort))
  (eq (ssterm~get-ssort :signs) ssort))

(defmethod post~print ((sgn-tuple sgn+tuple) stream)
  (format stream "(signs" (sgn=tuple-list sgn-tuple))
  (do ((sgn-tail sgn-tuple (sgn~tuple-rest sgn-tail)))
      ((sgn~tuple-empty-p sgn-tail))
    (format stream " ~A" (sgn~tuple-first sgn-tail)))
  (format stream ")"))

(defmethod post~read-object (post-signs (env env+environment) 
			    (indicator (eql :signs)))
  (let ((signs (mapcar #'(lambda (sgn)
			   (cond ((equal sgn '+) sgn*plus)
				 ((equal sgn '-) sgn*minus)
				 (t (error "~S is not a valid sign." sgn))))
		       post-signs)))
    (sgn~tuple-create signs)))

(defun sgn~tuple-plus-p (sgn-tuple)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A sign tuple.")
	   (effect  "None.")
	   (value   "True iff the sign tuple is positive, i.e. the number of minus signs is even."))
  (evenp (count sgn*minus (sgn=tuple-list sgn-tuple))))

(defun sgn~tuple-empty-p (sgn-tuple)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A sign tuple.")
	   (effect  "None.")
	   (value   "True iff the tuple is empty."))
  (null (sgn=tuple-list sgn-tuple)))


(defun sgn=tuple-create-from-poly-signed (sgns)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A list of poly signs.")
	   (effect  "None.")
	   (value   "All combinations of plus and minus according to the length of the input list."))
  (if sgns
      (append (mapcar #'(lambda (tuple) (cons sgn*plus tuple)) (sgn=tuple-create-from-poly-signed (rest sgns)))
	      (mapcar #'(lambda (tuple) (cons sgn*minus tuple)) (sgn=tuple-create-from-poly-signed (rest sgns))))
    (list nil)))

(defun sgn~tuple-create-from-poly-signed (sgns)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A list of poly signs.")
	   (effect  "None.")
	   (value   "All combinations of plus and minus according to the length of the input list (as objects)."))
  (mapcar 'sgn~tuple-create (sgn=tuple-create-from-poly-signed sgns)))
  
(defun sgn~create-tuples (sgns sign)
  (declare (edited  "25-JUN-2001")
	   (authors Scholl)
	   (input   "A sign list and a single sign.")
	   (effect  "None.")
	   (value   "All combinations of plus and minus according to sgns and sign."))
  (cond ((sgn~plus-p (first sgns)) (list (sgn~tuple-create nil)))
	((sgn~minus-p (first sgns)) (list (sgn~tuple-create nil)))
	((every #'(lambda (sgn) (sgn~poly-p sgn)) sgns)
	 (let ((sgn-tuples (sgn~tuple-create-from-poly-signed sgns)))
	   (if (sgn~plus-p sign)
	       (remove-if-not 'sgn~tuple-plus-p sgn-tuples)
	     (remove-if 'sgn~tuple-plus-p sgn-tuples))))
	(t (omega~error "Signs is no valid list: ~A" sgns))))
