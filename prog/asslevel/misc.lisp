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


(mod~defmod MISC 
            :uses (keim mod pdsn pp prob)
            :documentation "Miscellaneous"
            :exports (misc+hash-table
                      misc+hash-table
                      
                      misc~assumptions
                      misc~clear-hash-tables
                      misc~clrhash
                      misc~gethash
                      misc~hash-table
                      misc~hypotheses
                      misc~make-hash-table
                      misc~maphash
                      misc~nested-replace
                      misc~nil-in-list
                      misc~real-time-consume
                      misc~remhash
                      misc~run-time-consume
                      misc~show
                      misc~show-pretty
                      misc~store
                      misc~toplist
                      misc~union
                      ))

#+old(mod~defmod MISC 
            :uses (keim pp)
            :documentation "Miscellaneous"
            :exports (misc+hash-table
                      
                      misc~clear-hash-tables
                      misc~clrhash
                      misc~gethash
                      misc~hash-table
                      misc~make-hash-table
                      misc~maphash
                      misc~nested-replace
                      misc~nil-in-list
                      misc~real-time-consume
                      misc~remhash
                      misc~run-time-consume
                      misc~show
                      misc~show-pretty
                      misc~store
                      misc~toplist
                      misc~union))


;#########################################
;##                                     ##
;##             Hash tables             ##
;##                                     ##
;#########################################

(defclass misc+hash-table (keim+name)
  ((hash :reader misc~hash-table
	 :initform (make-hash-table :test #'equal)
	 :documentation
	 "This is the {hash table} slot of {the STORE module}."))
  (:documentation "This is the class of all hash tables of the STORE module."))

(defun misc~make-hash-table (name)
  (declare (edited  "14-FEB-1995")
	   (authors Afiedler)
	   (input   "A name.")
	   (effect  "Creates a new hash table with name NAME.")
	   (value   "The new hash table."))
  (make-instance 'misc+hash-table :name name))

(defgeneric misc~gethash (key hash)
  (declare (edited  "15-FEB-1994 14:48")
	   (authors AFIEDLER)
	   (input   "A symbol or string and a hash table.")
	   (value   "The entry in HASH, whose key is KEY, if it exists, otherwise NIL."))
  (:method (key (hash hash-table))
	   (gethash key hash))
  (:method (key (hash misc+hash-table))
	   (gethash key (misc~hash-table hash)))
  (:method ((key symbol) (hash misc+hash-table))
	   (gethash (string-upcase (symbol-name key)) (misc~hash-table hash)))
  (:method ((key string) (hash misc+hash-table))
	   (gethash (string-upcase key) (misc~hash-table hash))))

(defsetf misc~gethash (key hash) (value)
  "The setf-function to write entries into a hash table."
  `(let ((key ,key)
	 (hash ,hash)
	 (value ,value))
     (setf (gethash (typecase key
		      (symbol (string-upcase (symbol-name key)))
		      (string (string-upcase key))
		      (t key))
		    (etypecase hash
		      (misc+hash-table (misc~hash-table hash))
		      (hash-table hash)))
	   value)))

(defgeneric misc~maphash (func hash)
  (declare (edited  "14-FEB-1995")
	   (authors Afiedler)
	   (input   "A function and a hash table.")
	   (effect  "As {\\tt maphash}.")
	   (value   "As {\\tt maphash}."))
  (:method (func (hash hash-table))
	   (maphash func hash))
  (:method (func (hash misc+hash-table))
	   (maphash func (misc~hash-table hash))))

(defgeneric misc~remhash (key hash)
  (declare (edited  "15-FEB-1994 15:42")
	   (authors AFIEDLER)
	   (input   "A symbol or string and a hash table.")
	   (effect  "Removes any entry for KEY in HASH.")
	   (value   "T, if there was an entry, otherwise NIL."))
  (:method (key (hash hash-table))
	   (remhash key hash))
  (:method ((key symbol) (hash misc+hash-table))
	   (remhash (string-upcase (symbol-name key)) (misc~hash-table hash)))
  (:method ((key string) (hash misc+hash-table))
	   (remhash (string-upcase key) (misc~hash-table hash))))

(defgeneric misc~clrhash (hash)
  (declare (edited  "14-FEB-1995")
	   (authors Afiedler)
	   (input   "A hash table.")
	   (effect  "Clears the hash table.")
	   (value   "Undefined."))
  (:method ((hash hash-table))
	   (clrhash hash))
  (:method ((hash misc+hash-table))
	   (clrhash (misc~hash-table hash))))

(defun misc~clear-hash-tables (tables)
  (declare (edited  "14-FEB-1995")
	   (authors Afiedler)
	   (input   "A list of hash tables.")
	   (effect  "Clears all hash tables in TABLES.")
	   (value   "The cleared hash tables."))
  (dolist (hash tables tables)
    (misc~clrhash hash)))

(defgeneric misc~store (obj)
  (declare (edited  "04-FEB-1993 14:19")
	   (authors AFIEDLER)
	   (input   "A object.")
	   (effect  "Stores OBJ in the according hash table with its name as key.")
	   (value   "OBJ"))
  (:documentation "Stores a node in the according hash table"))




;#########################################
;##                                     ##
;##             Printing                ##
;##                                     ##
;#########################################

(defun misc=show-hash-table (hash)
  (declare (edited  "18-MAY-1995")
	   (authors Afiedler)
	   (input   "A hash table.")
	   (effect  "Show the contents of the hash table.")
	   (value   "Undefined."))
  (format t "~&~%Showing hash table ~A~%   Key~3,1TValue" hash)
  (misc~maphash #'(lambda (key value)
		     (format t "~&   ~A~3,1T~S" key value))
		 hash))

(defgeneric misc~show (obj &optional (style 'pp+top))
  (declare (edited  "04-JUL-1995")
	   (authors Afiedler)
	   (input   "An object.")
	   (effect  "OBJ is printed to {\\tw *standard-output*}.")
	   (value   "Undefined."))
  (:method (obj &optional (style 'pp+top))
	   (pp~pprint obj style))
  (:method ((obj hash-table) &optional style)
	   (declare (ignore style))
	   (misc=show-hash-table obj))
  (:method ((obj misc+hash-table) &optional style)
	   (declare (ignore style))
	   (misc=show-hash-table obj)))

(defgeneric misc~show-pretty (obj)
  (declare (edited  "06-MAR-1996")
	   (authors Afiedler)
	   (input   "An object.")
	   (effect  "OBJ is printed in a pretty style to {\\tw *standard-output*}.")
	   (value   "Undefined."))
  (:method (obj)
	   (misc~show obj)))


(pp~defstyle misc-nice
	     :help "A nice printing style"
	     )


;#########################################
;##                                     ##
;##        Handling of Lists            ##
;##                                     ##
;#########################################

(defun misc~nested-replace (l old new &key (test #'eq))
  (declare (edited  "29-MAY-1995")
	   (authors Afiedler)
	   (input   "A list, an old item, a new one, and optionally a test function.")
	   (effect  "Replaces OLD by NEW in L.")
	   (value   "The modified list."))
  (mapcar #'(lambda (el)
	      (cond ((listp el) (misc~nested-replace el old new :test test))
		    ((funcall test el old) new)
		    (t el)))
	  l))

(defun misc~toplist (l)
  (declare (edited  "01-MAR-1993 17:01")
	   (authors AFIEDLER)
	   (input   "A list.")
	   (effect  "Takes the symbols in L to the top of the list, i. e. erases the"
		    "parantheses exept the outermost.")
	   (value   "The new list."))
  (if l
      (if (cdr l)
	  (if (listp (car l))
	      (misc~toplist (append (car l) (cdr l)))
	    (append (list (car l)) (misc~toplist (cdr l))))
	(if (listp (car l))
	    (misc~toplist (car l))
	  l))))

(defun misc~nil-in-list (l)
  (declare (edited  "26-AUG-1993 10:47")
	   (authors AFIEDLER)
	   (input   "A list.")
	   (value   "T if nil occurs in L."))
  (if (listp l)
      (if (some #'null l)
	  t
	(dolist (el l)
	  (if (misc~nil-in-list el)
	      (return t))))))

(defun misc~union (lists)
  (declare (edited  "25-AUG-1993 13:35")
	   (authors AFIEDLER)
	   (input   "A list of lists.")
	   (value   "The union of the lists."))
  (let ((first-list (car lists))
	(rest-lists (cdr lists)))
    (if rest-lists
	(misc~union (append (list (union first-list (car rest-lists))) (cdr rest-lists)))
      first-list)))



;#########################################
;##                                     ##
;##           Time-consume              ##
;##                                     ##
;#########################################


(defun misc~run-time-consume (function &rest arguments)
  (declare (edited  "01-OCT-1996")
	   (authors Afiedler)
	   (input   "A function and its arguments.")
	   (effect  "The run-time consume of FUNCTION applied on ARGUMENTS is determined.")
	   (value   "The result of the application of FUNCTION on ARGUMENTS." ))
  (let ((start (get-internal-run-time))
	(val (apply function arguments)))
    (format t "~&~%Time consume: ~A" (- (get-internal-run-time) start))
    val))

(defun misc~real-time-consume (function &rest arguments)
  (declare (edited  "01-OCT-1996")
	   (authors Afiedler)
	   (input   "A function and its arguments.")
	   (effect  "The real-time consume of FUNCTION applied on ARGUMENTS is"
		    "determined.") 
	   (value   "The result of the application of FUNCTION on ARGUMENTS."))
  (let ((start (get-internal-real-time))
	(val (apply function arguments)))
    (format t "~&~%Time consume: ~S seconds" (/ (- (get-internal-real-time) start)
						internal-time-units-per-second))
    val))



;### others

(defun misc~hypotheses (proof)
  (declare (edited  "24-NOV-1997")
	   (authors Afiedler)
	   (input   "A proof plan.")
	   (value   "The list of hypotheses, that are necessary to prove the conclusion in"
		    "PROOF."))
  (labels ((asses (line)
		  (if (pdsn~hypothesis-p line)
		      (list line)
		    (apply #'append (mapcar #'asses (pdsn~just-premises line))))))
    (remove-duplicates (asses (prob~proof-root proof)))))

(defun misc~assumptions (proof)
  (declare (edited  "24-NOV-1997")
	   (authors Afiedler)
	   (input   "A proof plan.")
	   (value   "The list of assumptions, that are necessary to prove the conclusion"
		    "in PROOF."))
  (let ((proof-ass (misc~hypotheses proof)))
    (apply #'append
	   (mapcar #'(lambda (x)
		       (let ((res (find x proof-ass
					:test #'(lambda (a b)
						  (string-equal (keim~name a)
								(keim~name b))))))
			 (if res (list res))))
		   (prob~assumptions proof)))))
