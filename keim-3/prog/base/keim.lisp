;;; -*- Mode: LISP; Base: 10; Syntax: Common-lisp; Package: KEIM -*-

(in-package "KEIM")

(mod~defmod keim :uses (mod )
	    :documentation "Abstract datatype for the superclass of all KEIM objects."
	    :exports (
		      keim+object
		      keim+name
		      keim+term
		      keim~equal
		      keim~copy
		      keim~name
		      keim~set-name!
		      keim~plist
		      keim~set-plist!
		      keim~get
		      keim~put
		      keim~remprop
		      keim~remprops
		      )
	    )



#{\section{\keim\ objects} 

The class {\vb keim+object} is a superclass to all \clos\ objects in \keim. Thus all \keim\ objects inherit
the functionality defined here. Its subclass {\vb keim+name} is the superclass for all objects with a name.
The class {\vb keim+term} is the superclass to shared-symbols, terms and environments of normal
terms. It is used when a new symbol is created ~ to determine the right class from the reference term.#}


#{\subsection{General Keim Functions}#}

(eval-when (load compile eval)
  (defclass keim+object ()
      ((plist :initform nil :reader keim=plist :writer keim=write-plist!))
    (:documentation "This is the superclass to all other classes.")))

(eval-when (load compile eval)
  (defclass keim+name (keim+object)
    ((name :initarg :name :reader keim=name :writer keim=write-name! 
	   :initform nil))
    (:documentation "This is a mixin class for all KEIM objects with names.")))

#{The following class is used for the reference term
mechanism in creating and copying terms for documentation consult the documentation of  the {\vb term}
system. This module only supplies the class and gives it no functionality, therefore it can be skipped for the
purposes of this module.#}

(eval-when (load compile eval)
  (defclass keim+term (keim+object)
      ()
    (:documentation "This is the superclass to shared-symbols, terms, ~
                     and environments of normal terms. It is used when a new symbol is created ~
                     to determine the right class from the reference term.")))

#{{\vb KEIM~EQUAL} is a function that can check for equality of \keim\ objects in a sensible way. Here we
define the interface function the actual methods are defined, where the objects are defined. If there is no
method definition, {\vb KEIM~EQUAL} defaults to {\vb EQ} #}

(defgeneric keim~equal (keim-object1 keim-object2)
  (declare (edited  "06-DEC-3707 14:65")
	   (authors RICHTS)
	   (input   "Two objects or object lists.")
	   (effect  "None.")
	   (value   "True, iff the two objects are equal."))
  (:method (object1 object2)
	   (eq object1 object2))
  (:method ((keim-object1 keim+object) (keim-object2 keim+object))
	   (eq keim-object1 keim-object2))
  (:method  ((object1 cons) (object2 cons))
	    (and (keim~equal (car object1) (car object2))
		 (keim~equal (cdr object1) (cdr object2))))
  (:method ((termlist1 list) (termlist2 list))
	   (and (= (length termlist1) (length termlist2))
		(every #'keim~equal termlist1 termlist2)))
  (:method ((object1 string) (object2 string))
	   (string= object1 object2)))

;;;inserted on behalf of Armin's observation;   DEF:

(defmethod keim~equal :around (ob1 ob2)
  (if (equal (type-of ob1) (type-of ob2))
      (call-next-method)))

(defgeneric keim~copy (keim-object &key (explode :all-classes) share preserve downto)
  (declare (edited  "06-DEC-1991 12:54")
	   (authors RICHTS AMEIER)
	   (input   "A object.")
	   (effect  "None.")
	   (value   "A copy of KEIM-OBJECT."))
  (:method ((termlist list) &key (explode :all-classes) share preserve downto)
	   (mapcar #'(lambda (item)
		       (keim~copy item
				  :explode explode
				  :share share
				  :preserve preserve
				  :downto downto))
		   termlist)))

#{\keim\ names can be strings or symbols, but symbols are always preferable, since the case conflicts do not
arise.#}

(defgeneric keim~name (keim-object)
  (declare (edited  " 3-JUL-1992 11:32" )
	   (authors KOHLHASE )
	   (input   "An object.")
	   (effect  "None.")
	   (value   "If KEIM-OBJECT has a name, then the name otherwise a string representation of the object."))
  (:method ((object keim+name))
   (keim=name object))
  (:method ((object t))
   (error "(KEIM) The object ~S does not have a name for KEIM~~NAME." object)))

(defgeneric keim~set-name! (keim-object name)
  (declare (edited  " 3-JUL-1992 11:32" )
	   (authors KOHLHASE )
	   (input   "An object and a new name.")
	   (effect  "If KEIM-OBJECT has a name-slot, then that is set to NAME, otherwise error.")
	   (value   "The KEIM-OBJECT with the new name NAME."))
  (:method ((object keim+name) name)
   (keim=write-name! name object))
  (:method ((object t) name)
   (declare (ignore name))
   (error "The object ~S does not have a name slot for KEIM~~SET-NAME!." object)))


#{\subsection{Property-Lists}
\keim\ provides its own property list facility for all objects. These have the same functionality as
\commonlisp\ property lists see any \commonlisp\ docmuentation or introduction for comments.#}

(defgeneric keim~plist (keim-object)
  (declare (edited  "11-SEP-1992 14:06")
	   (authors RICHTS)
	   (input   "A KEIM-object.")
	   (effect  "None.")
	   (value   "The property list of this object."))
  (:method ((anything keim+object))
	   (keim=plist anything)))

(defgeneric keim~set-plist! (keim-object plist)
  (declare (edited  "11-SEP-1992 14:11")
	   (authors RICHTS)
	   (input   "A KEIM-object and list of associations.")
	   (effect  "The property list of KEIM-OBJECT is set to PLIST.")
	   (value   "The new property-list of KEIM-OBJECT, i.e. PLIST."))
  (:method ((keim-object keim+object) plist)
   (keim=write-plist! plist keim-object)))

(defun keim~get (keim-object indicator &optional default)
  (declare (edited  "06-DEC-1991 12:57")
	   (authors RICHTS)
	   (input   "A KEIM object, a symbol and a default-value.")
	   (effect  "None.")
	   (value   "The value to INDICATOR in the property-list of KEIM-OBJECT,"
		    "or DEFAULT if INDICATOR isn't found."))
  (do ((plist (keim~plist keim-object) (cddr plist)))
      ((or (eql (car plist) indicator) (null plist))
       (if plist (cadr plist) default))))

(defun keim~put (keim-object indicator value)
  (declare (edited  "06-DEC-1991 13:19")
	   (authors RICHTS)
	   (input   "A KEIM object, a symbol and the new value.")
	   (effect  "The property-list of KEIM-OBJECT is changed by adding the new property-value pair"
		    "of INDICATOR and VALUE or by replacing the old value for INDICATOR.")
	   (value   "VALUE."))
  (do ((plist (keim~plist keim-object) (cddr plist)))
      ((or (eql (car plist) indicator) (null plist))
       (if plist
	   (setf (cadr plist) value)
	   (keim~set-plist! keim-object (cons indicator (cons value (keim~plist keim-object)))))))
  value)

(defun keim~remprop (keim-object indicator)
  (declare (edited  "06-DEC-1991 13:20")
	   (authors RICHTS)
	   (input   "A KEIMobject and a symbol.")
	   (effect  "The property-value pair for INDICATOR is removed from the property-list of KEIM-OBJECT.")
	   (value   "True, iff INDICATOR was an indicator in the property-list of KEIM-OBJECT."))
  (keim~remprops keim-object (list indicator)))

(defun keim~remprops (keim-object indicators)
  (declare (edited  "06-DEC-1991 13:28")
	   (authors RICHTS)
	   (input   "A KEIM object and a list of symbols.")
	   (effect  "The property-value pairs for the indicators in INDICATORS are removed"
		    "from the property-list of KEIM-OBJECT.")
	   (value   "True, iff every member in INDICATORS was an indicator in the property-list of KEIM-OBJECT."))
  (let* ((removed-indicators nil)
	 (plist (do ((plist-tail (keim~plist keim-object) (cddr plist-tail)))
		    ((or (null plist-tail) (not (member (car plist-tail) indicators)))
		     plist-tail)
		  (push (car plist-tail) removed-indicators))))
    (keim~set-plist! keim-object plist)
    (do ((plist-tail plist))
	((null (cddr plist-tail)))
      (if (member (caddr plist-tail) indicators)
	  (progn (push (caddr plist-tail) removed-indicators)
		 (pop (cddr plist-tail))
		 (pop (cddr plist-tail)))
	  (setf plist-tail (cddr plist-tail))))
    (every #'(lambda (indicator) (member indicator removed-indicators)) indicators))) 

(defmethod print-object ((obj keim+name) stream)
  (format stream "#<~A ~A>" (type-of obj) (keim~name obj)))
