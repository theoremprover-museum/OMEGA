;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(in-package :keim)
 
(mod~defmod help :uses (mod  keim)
	    :documentation "Definition of help facilities."
	    :exports (help+help
		      help~help-string
		      help~fetch-all-help
		      help~fetch
		      ))

#{\section{Help objects}

Objects which are of type {\tt HELP+HELP} are named objects with which a a help string can be associated in
the HELP slot. With this facility the implementor can give help information for individual objects of class
{\tt HELP+HELP} when they are created. When such an object is defined, it will be stored in a central database
under its name.  Redefining such an object of the same type with the same name causes the first to be
removed.

The class {\vb HELP+HELP} is a mixin-class, that provides the \keim\ help facility. To prepare a class of objects
for help just add the class{\vb HELP+HELP} to its list of superclasses.#}


(eval-when (load compile eval)
  (defclass help+help (keim+name)
    ((help :initarg :help :initform "" :accessor help=help
	   :documentation "A string for help."))
    (:documentation "A mixin class for providing help on KEIM objects.")))

(defvar help*help-hash-table (make-hash-table :test #'equal))
#{The function {\vb HELP~HELP-STRING} returns the help string of a particular object, whereas the function
{\vb HELP~FETCH-ALL-HELP} and {\vb HELP~FETCH} search for objects with a given name (and class).

\begin{code}
KEIM(1): (defclass test (help+help) ())
#<STANDARD-CLASS TEST @ #xae514e>
KEIM(2): (setq t1 (make-instance 'test :help "This is the first test object"
				       :name "first"))
#<TEST @ #xaf0896>
KEIM(3): (setq t2 (make-instance 'test :help "This is the second test object"
				       :name "second"))
#<TEST @ #xaf007e>
KEIM(4): (setq fir (first (help~fetch-all-help "first")))
#<TEST @ #xaf0896>
[14] KEIM(85): (help~help-string fir)
"This is the first test object"
KEIM(5): (help~help-string (first (help~fetch-all-help "second")))
"This is the second test object"
\end{code}
Note that the names of objects are used as case-insensitive strings. In
particular, if two objects have symbol names which differ only in their
home packages, they will be identified.
#}


(defgeneric help~help-string (obj)
  (declare
   (authors nesmith)
   (input "Any object of type HELP+HELP")
   (value "The object's help string."))
  (:method ((obj help+help))
   (help=help obj)))

(defun help=insert (key object)
  (declare
   (authors nesmith)
   (input "A KEY (symbol or string) and an object.")
   (effect "The object is stored in the help*help-hash-table under the
KEY.  If an object of the same class is already stored under this KEY,
then it will be removed.  KEY is used as a string, so it is case-insensitive
and independent of package.")
   (value "NIL."))  
  (when (symbolp key) (setq key (symbol-name key)))
  (let* ((object-class (class-of object))
	 (other-helps 
	  (delete-if 
	   #'(lambda (x) (eq (class-of x) object-class))
	   (gethash key help*help-hash-table nil))))
    (setf (gethash key help*help-hash-table)
	  (cons object other-helps))
    nil))

(defun help~fetch-all-help (key)
  (declare
   (authors nesmith)
   (input "A KEY (symbol or string).")
   (effect "All objects which are of type HELP+HELP defined with the name 
KEY will be retrieved.  KEY is used a string, so it is case-insensitive and
independent of package.")
   (value "The list of objects or NIL."))
  (when (and key (symbolp key)) (setq key (symbol-name key)))
  (gethash key help*help-hash-table))

(defgeneric help~fetch (key class)
  (declare
   (authors nesmith)
   (input "A KEY (symbol or string) and a CLASS (symbol naming a CLOS class).")
   (effect "None.")
   (value "If an object of the given CLASS is defined under the name KEY, it
will be returned, otherwise NIL."))
  (:method (key class)
   (declare (ignore key class))
   (error "Don't know how to get help with these args"))
  (:method ((key symbol) class)
   (help~fetch (symbol-name key) class))
  (:method (key (class symbol))
   (help~fetch key (find-class class)))
  (:method ((key symbol) (class symbol))
   (help~fetch (symbol-name key) class))
  (:method ((key string) (class symbol))
   (let ((all-help (help~fetch-all-help key)))
     (find class all-help :key #'(lambda (x) (class-name (class-of x))))))
)



(defmethod shared-initialize :after ((obj help+help) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (setf (help=help obj) (help=normalize-help-string (help=help obj)))
  (help=insert (keim~name obj) obj)
  obj)


(defun help=normalize-help-string (string)
  string)
;  (declare 
;   (authors nesmith)
;   (input "a string")
;   (effect "none")
;   (value "the input string has all extra spaces removed, all newlines and tabs
;are replaced by spaces, and new string is returned"))
;  (labels ((remove-double-space (str char)
;             (cond ((zerop (length str)) (string char))          
;                   ((and (char= #\space char) 
;                         (char= #\space (char str (1- (length str)))))
;                    str)
;                   (t (concatenate 'string str (string char)))))
;           (add-space-after-punct (string)
;             (if (member (char string (1- (length string)))
;                         '(#\, #\. #\; #\?)
;                         :test #'char=)
;                 (concatenate 'string string " ")
;               string))
;           (split-string (string)
;             (let ((res nil))
;               (do* ((string 
;                      (reduce #'remove-double-space
;                              (substitute-if #\space 
;                                             #'(lambda (ch)
;                                                 (member ch '(#\tab #\newline)
;                                                         :test #'char=))
;                                             string)
;                              :initial-value "")
;                      (subseq string (1+ n)))
;                     (n (position-if #'(lambda (ch) 
;                                         (member ch '(#\space #\. #\, #\; #\?)
;                                                 :test #'char=))
;                                     string)
;                        (position-if #'(lambda (ch) 
;                                         (member ch '(#\space #\. #\, #\; #\?)
;                                                 :test #'char=))
;                                     string)))
;                   ((not n) (mapcar #'add-space-after-punct
;                                    (delete " "
;                                            (nreverse 
;                                             (if (zerop (length string))
;                                                 res
;                                               (push string res)))
;                                            :test #'string-equal)))
;                 (push
;                  (subseq string 0 (1+ n))
;                  res)))))
;    (let ((newstrings (split-string string)))
;      (apply #'concatenate 'string newstrings)   
;      )))
  

