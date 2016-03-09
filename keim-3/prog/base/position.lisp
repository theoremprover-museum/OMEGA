;;; -*- Package: KEIM; Syntax: Common-lisp; Mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
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

(in-package "KEIM")

(mod~defmod pos :uses (env keim mod post)
	      :documentation "Abstract datatype for the KEIM position facility."
	      :exports (
			pos+position
			pos~p
			pos~empty
			pos~add-front
			pos~add-end
			pos~empty-p
			pos~first
			pos~set-first!
			pos~rest
			pos~set-rest!
			pos~add-end!
			pos~next!
			pos~number-list
			pos~list-position
			pos~prefix-p
			pos~gcp
			pos~butlast
			pos~last
			pos~concatenate
			pos~int-p
			pos~read

			)
	      )





#{
\section{Term Positions}\label{mod:pos}
Positions are the key mechanism to reference substructures in structures. Most \keim\ objects
support the universal \keim\ position mechanism.
The POST syntax for a position is {\tt (position posinteger*)}.
#}



(eval-when (load compile eval)
(defclass pos+position (keim+object)
    ((first :initarg :first :reader pos=first :writer pos=write-first!)
     (rest :initarg :rest :reader pos=rest :writer pos=write-rest!))
    (:documentation "The class of all positions in structures")))

(defun pos=create (&key first rest)
  (declare (edited  "23-AUG-1991 19:22")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   ))
  (make-instance 'pos+position :first first :rest rest))

(defun pos~p (obj)
  (typep obj 'pos+position))

(defmethod print-object ((position pos+position) stream)
  (declare (edited  "23-AUG-1991 19:22")
	   (authors RICHTS)
	   (input   )
	   (effect  )
	   (value   ))
  (declare (special *print-length*))
  (cond ((pos~empty-p position)
	 (format stream "( )"))    ;;;; wegen interaction der agenten mit LUI bitte runde
				   ;;;; Klammern in print Funktion verwenden , CB
	(t (format stream "(~A" (pos~first position))
	   (do* ((i 1 (1+ i))
		 (position-tail (pos~rest position) (pos~rest position-tail)))
		((or (and *print-length* (eql i *print-length*))
		     (pos~empty-p position-tail))
		 (if (pos~empty-p position-tail)
		     (format stream ")")
		     (format stream " ...)")))
	     (format stream " ~A" (pos~first position-tail))))))


;; To use positions efficiently during walking through a term they should be build from back to front like lists.

(defmethod keim~copy ((position pos+position) &key (explode :all-classes) share preserve downto)
  (declare (edited  "29-AUG-1991 13:49")
	   (authors RICHTS)
	   (input   "A position.")
	   (effect  "None.")
	   (value   "A copy of POSITION."))
  (if (or (eq downto :all-classes) (some #'(lambda (x) (typep position x)) downto))
      position
    (if (pos~empty-p position)
	(pos~empty)
      (pos~add-front (pos~first position) (keim~copy (pos~rest position) :downto nil)))))

(defmethod keim~equal ((position1 pos+position) (position2 pos+position))
  (declare (edited  "29-AUG-1991 13:49")
	   (authors RICHTS nesmith)
	   (input   "Two positions.")
	   (effect  "None.")
	   (value   "True iff POSITION1 and POSITION2 are equal."))
  (cond ((eq position1 position2) t)
	((pos~empty-p position1) (pos~empty-p position2))
	((pos~empty-p position2) nil)
	(t
	 (and (= (pos~first position1) 
		 (pos~first position2))
	      (keim~equal (pos~rest position1) 
			  (pos~rest position2))))))
	 
#{
\subsection{Constructors}
#}

(defun pos~empty ()
  (declare (edited  "29-AUG-1991 13:45")
	   (authors RICHTS)
	   (input   "None.")
	   (effect  "None.")
	   (value   "An empty position.")
	   (example "(pos~empty) --> [ ]"))
  (pos=create :first nil :rest nil))

(defun pos~add-front (number &optional position)
  (declare (edited  "23-AUG-1991 19:14")
	   (authors RICHTS)
	   (input   "A number and optionally a position.")
	   (effect  "None.")
	   (value   "NUMBER is 'consed' in front of POSITION (or the empty position,
                     if no POSITION is supplied). The result is
                     the new position [NUMBER ..POSITION].")
	   (example "5  [4 3] --> (5 4 3)"))
  (if position
      (pos=create :first number :rest position)
      (pos=create :first number :rest (pos~empty))))

(defun pos~add-end (number &optional position)
  (declare (edited  "23-AUG-1991 19:14")
	   (authors RICHTS)
	   (input   "A number and optionally a position.")
	   (effect  "None.")
	   (value   "NUMBER is added at the end of a copy of POSITION (or the empty position,
                     if no POSITION is supplied). The result is
                     the new position [..POSITION NUMBER].")
	   (example "7 [5 4 3] --> [5 4 3 7]"))
  (if (or (null position) (pos~empty-p position))
      (pos~add-front number)
      (pos~add-front (pos~first position) (pos~add-end number (pos~rest position)))))

      


#{
\subsection{Predicates}
#}

(defun pos~empty-p (position)
  (declare (edited  "23-AUG-1991 19:22")
	   (authors RICHTS)
	   (input   "A position.")
	   (effect  "None.")
	   (value   "True iff POSITION is empty.")
	   (example "[ ] --> T"))
  (null (pos~first position)))

#{
\subsection{Accessors}
#}

(defun pos~first (position)
  (declare (edited  "23-AUG-1991 19:20")
	   (authors RICHTS)
	   (input   "A position.")
	   (effect  "None.")
	   (value   "The first number of POSITION or nil if it is empty.")
	   (example "[5 4 3 7] --> 5"))
  (pos=first position))

(defun pos~set-first! (position number)
  (declare (edited  "23-AUG-1991 19:22")
	   (authors HUANG RICHTS)
	   (input   "A position POSITION and a number NUMBER.")
	   (effect  "The first number in POSITION is changed to NUMBER.")
	   (value   "The changed Position")
	   (example "Let pos1 be [5 4 3 7],"
		     "(pos~set-first pos1 78) --> [78 4 3 7]"
		     "pos1 --> 78"))
  (pos=write-first! number position)
  position)


(defun pos~rest (position)
  (declare (edited  "23-AUG-1991 19:20")
	   (authors RICHTS)
	   (input   "A position.")
	   (effect  "None.")
	   (value   "The position without the first number of POSITION.")
	   (example "[78 4 3 7] --> [4 3 7]"))
  (if (pos~empty-p position)
      (error "Position is empty")
      (pos=rest position)))

(defun pos~set-rest! (position position-tail) ;;;noch falsch
  (declare (edited  "23-AUG-1991 19:22")
	   (authors RICHTS)
	   (input   "A position POSITION and a list POSITION-TAIL of numbers.")
	   (effect  "The rest in POSITION is changed to POSITION-TAIL.")
	   (value   "The changed Position")
	   (example "Let pos1 be [78 4 3 7]"
		     "(pos~set-rest! pos1 '(1 2) --> [78 1 2]"
		     "pos1 --> [78 1 2]"))
  (if (pos~empty-p position)
      (error "Position is empty")
    (pos=write-rest! position-tail position)))


#{
\subsection{Auxiliary functions}
#}


(defun pos~add-end! (number &optional position)
  (declare (edited  "23-AUG-1991 19:14")
	   (authors RICHTS)
	   (input   "A number and a position or nil.")
	   (effect  "NUMBER is added destructively at the end of POSITION.")
	   (value   "The changed POSITION if there was one, or a new position [NUMBER] else.")
	   (example "let pos1 be [5 4 3 7 8]"
		     "(pos~add-end! 8 pos1) --> [5 4 3 7 8]"
		     "pos1 --> [5 4 3 7 8]"))
  (cond ((null position)
	 (pos~add-front number))
	((pos~empty-p position)
	 (pos~set-first! position number)
	 (pos~set-rest! position (pos~empty))
	 position)
	(t (pos~add-end! number (pos~rest position))
	   position)))


(defun pos~next! (position)
  (declare (edited  "23-AUG-1991 19:18")
	   (authors RICHTS)
	   (input   "A non-empty position.")
	   (effect  "POSITION is destroyed.")
	   (value   "The last number of POSITION is incremented by one.")
	   (example "[5 4 3 7 8] --> [5 4 3 7 9]"))
  (cond ((pos~empty-p position)
	 (error "Position is not allowed to be empty."))
	((pos~empty-p (pos~rest position))
	 (pos~set-first! position (+ (pos~first position) 1))
	 position)
	(t (pos~next! (pos~rest position))
	   position)))

(defun pos~number-list (position)
  (declare (edited  "29-OCT-1991 10:38")
	   (authors RICHTS)
	   (input   "A position POSITION.")
	   (effect  "None.")
	   (value   "A list of numbers eqivalent to POSITION.")
	   (example "[5 4 3 7 8] --> (5 4 3 7 8)"))
  (if (pos~empty-p position)
      nil
      (cons (pos~first position) (pos~number-list (pos~rest position)))))
 
(defun pos~list-position (list)
  (declare (edited  "22-DEC-1991 10:38")
	   (authors NESMITH)
	   (input   "A list of integers.")
	   (effect  "None.")
	   (value   "A position which has these integers as elements.")
	   (example "(1 2 3) --> [1 2 3]"))
  (let ((new-pos (pos~empty)))
    (dolist (num list new-pos)
      (pos~add-end! num new-pos))))


#{
\subsection{Prefixes}
#}

(defun pos~prefix-p (position1 position2)
  (declare (edited  "29-AUG-1991 13:51")
	   (authors RICHTS)
	   (input   "Two positions.")
	   (effect  "None.")
	   (value   "True iff POSITION1 is a prefix of POSITION2.")
	   (example "[5 4 3] [5 4 3 7 9] --> T"))
  (cond ((eq position1 position2) t)
	((pos~empty-p position1) t)
	((pos~empty-p position2) nil)
	(t (and (= (pos~first position1) (pos~first position2))
		(pos~prefix-p (pos~rest position1) (pos~rest position2))))))

(defun pos~gcp (position1 position2)
  (declare (edited  "29-AUG-1991 13:49")
	   (authors RICHTS)
	   (input   "Two positions.")
	   (effect  "None.")
	   (value   "The greatest common prefix of POSITION1 and POSITION2.")
	   (example "[5 4 3] [5 4 3 7 9] --> [5 4 3]"))
  (if (or (pos~empty-p position1) (pos~empty-p position2) (/= (pos~first position1) (pos~first position2)))
      (pos~empty)
      (pos~add-front (pos~first position1) (pos~gcp (pos~rest position1) (pos~rest position2)))))

(defun pos~butlast (position &optional (n 1))
  (declare (edited  "22-Dec-92 13:49")
	   (authors NESMITH)
	   (input   "A position and optional integer N.")
	   (effect  "None.")
	   (value   "A new position with all elements except the last N; if N
is larger than the number of positions, then an empty position will be returned. N defaults to 1.")
	   (example "[5 4 3 7 9] 4 --> [5]"))
  (let ((new-pos (pos~empty))
	(nums (butlast (pos~number-list position) n)))
    (dolist (num nums new-pos)
      (pos~add-end! num new-pos))))
 
(defun pos~last (position &optional (n 1))
  (declare (edited  "22-Dec-92 13:49")
	   (authors NESMITH)
	   (input   "A position and optional integer N.")
	   (effect  "None.")
	   (value   "A new position with only the last N elements; if N
is larger or equal to the number of elements, then a copy of the original 
position will be returned.  If N is 0, then an empty
position is returned.  N defaults to 1.")
	   (example "[5 4 3 7 9] 2 --> [7 9]"
		     "[5 4 3 7 9] 6 --> [5 4 3 7 9]"))
  (let ((new-pos (pos~empty))
	; old common lisps don't allow optional arg to LAST, so we do this
	; roundabout
	(nums 
	 (let* ((nums (pos~number-list position))
		(m (- (length nums) n)))
	   (nreverse (nbutlast (nreverse nums) (if (>= m 0) m 0))))))
    (dolist (num nums new-pos)
      (pos~add-end! num new-pos))))
 
(defun pos~concatenate (&rest positions)
  (declare (edited  "23-Dec-92 13:49")
	   (authors NESMITH)
	   (input   "Any number of positions.")
	   (effect  "None.")
	   (value   "A new position which has all the elements of the
input positions, in the same order.  Basically just concatenates
the elements together and makes a new position.")
	   (example "[5 4 3 7 9] [5 4 3] --> [5 4 3 7 9 5 4 3]"))
    (let ((nums (apply #'concatenate 'list 
		       (mapcar #'pos~number-list positions))))
      (pos~list-position nums)))
  

(defun pos~int-p (thing)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A lisp object.")
	   (effect  "none")
	   (value   "T if the object is a nonnegative integer (suitable for use
in a position).")
	   (example "5 --> T, 0 --> T"))
  (and (integerp thing)
       (not (minusp thing))))

#{
\subsection{POST interface}
#}


(defmethod post~print ((position pos+position) stream)
  (format stream "(position")
  (do ((position-tail position (pos~rest position-tail)))
      ((pos~empty-p position-tail))
    (format stream " ~A" (pos~first position-tail)))
  (format stream ")"))

(defun pos~read (pos env)
  (declare (edited  "09-FEB-1993 12:46")
	   (authors nesmith)
	   (input   "A position POS in POST format and an environment.")
	   (effect  "The position is constructed.")
	   (value   "The new position is returned.")
	   (example "Let evn be an environment"
		     "(position 1 2 6 5) evn --> [1 2 6 5]"))
  (post~read-object (cdr pos) env :position))

(defmethod post~read-object (pos (env env+environment) 
			    (indicator (eql :position)))
  (let ((nums (mapcar #'(lambda (x) 
			  (unless (pos~int-p x)
			    (error "~S is not a valid integer ~
                                   for a position." x)) 
			  x)
		      pos)))
    (pos~list-position nums)))

