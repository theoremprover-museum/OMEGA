;;; -*- syntax: common-lisp; package: keim; base: 10; mode: lisp -*-
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

(in-package :keim)




(mod~defmod DATIX 
            :uses (data env keim post)
            :documentation "Embedding data structures in keim objects."
            :exports (datix+mixin                      
                      datix+named-struct                 
                      
                      datix~named-struct-p              
                      datix~struct
		      
                      datix~named-struct-abbrev        
                      datix~read-named-struct           
                      ))

#{
\section{Data structures as mixins}\label{mod:datix}
 Often we would like to have objects which behave like the keim general data strucures, but also
 have other properties (such as names).  Here DATIX+MIXIN is a new 
 KEIM+OBJECT
 that contains a slot STRUCT, where a structure can be placed.
#}


(eval-when (load compile eval)
(defclass datix+mixin (data+top keim+object)
  ((struct :initform nil :initarg :struct :accessor datix=struct))
  (:documentation "An object which contains a structure as a slot.")
  )
)


;;; ANDREAS, wie ginge das mit dem keim~defclass?

(defun datix~struct (object)
  (declare (edited  "19-JAN-1998")
           (authors Fehrer)
           (input   "an object")
           (effect  "none")
           (value   "the contained data structure"))
  (datix=struct object))

(defun (setf datix~struct) (new-struct object)
  (declare (edited  "19-JAN-1998")
	   (authors Fehrer)
	   (input   "a data structure and an object")
	   (effect  "sets the data structure contained in object to new-struct")
	   (value   "the new struct"))
  (setf (datix=struct object) new-struct))


(defmethod data~reader ((object datix+mixin))
  (datix~struct object))

(defmethod print-object ((thing datix+mixin) stream)
  (print-object (datix~struct thing) stream))

#{
\section{Named data structures}
 Here we define DATIX+NAMED-STRUCT  Instances of this class will
 contain a name slot as well as a structure slot, so we can associate
 names with structures.
#}

(eval-when (load compile eval)
(defclass datix+named-struct (datix+mixin keim+name)
    ()
  (:documentation "This is the superclass to all data structures in KEIM that have a name")))


(defun datix~named-struct-p (thing)
  (declare
   (authors nesmith)
   (input   "An object")
   (effect  "none")
   (value   "T if the object is a named data structure, otherwise nil."))
  (typep thing 'datix+named-struct))

(defmethod print-object :around ((named-struct datix+named-struct) stream)
	   (format stream "(~A ~S " (datix~named-struct-abbrev named-struct) 
		   (keim~name named-struct))
	   (call-next-method)
	   (format stream ")"))


(defgeneric datix~named-struct-abbrev (named-struct)
  (declare 
   (authors nesmith)
   (input   "A named-struct")
   (effect  "none")
   (value   "A short string identifying the specific type of the named-struct 
(used in printing)."))
  (:method ((thing datix+named-struct))
	   "named structure")
  (:documentation "A string abbreviation for this type of named of struct. Used in printing."))


;;; ANDREAS, was davon wird fuer die Einleserei usw. gebraucht?


(defun datix~read-named-struct (symbol env)
  (declare 
   (authors nesmith)
   (input   "A symbol and a environment")
   (effect  "none")
   (value   "If the symbol is associated with a named-struct in the environment,
returns the named-struct."))
  (let ((obj (env~lookup-object symbol env)))
    (unless (datix~named-struct-p obj)
      (post~error "~A is not a named struct in environment." symbol))
    obj))


(defmethod env~post-print (key (struct datix+named-struct) stream)
  (declare (ignore key))
  (post~print struct stream)
  (values))

