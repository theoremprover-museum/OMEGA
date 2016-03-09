;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: LISP -*-
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

(in-package :omega)


;; This is the module which defines a blackboard.
;;
;; A blackboard itself is simply a data-structure which has a name and one slot blackboard-table which contains a
;; hashatble in which blackboard-objects can be stored and accessed by its name.
;;
;; On the blackboard blackboard-objects can be placed. A blackboard object consists of a name, a content and blackboard-object-type. 
;;
;; To define a blackboard object type one has to specify a check-function (to check whether an object is of this type), a save-function
;; (how can an object of this type be stored in a file) and load-function (how can a stored object again restored from a file).
;;
;; The whole blackboard can then be saved and loaded simply as a list of names associated with objects.



(mod~defmod BLACK 
            :uses (keim omega post pp)
            :documentation "The data structures needed for blackboards"
            :exports (
                      black+blackboard
                      black+blackboard-object
                      black+blackboard-object-type
                      
                      black~add-new-blackboard-object!
                      black~all-blackboard-object-names-of-blackboard
                      black~all-blackboard-objects-of-blackboard
                      black~blackboard-object-p
                      black~blackboard-object-type-p
                      black~blackboard-p
                      black~blackboard-table
                      black~blob-content
                      black~blob-type
                      black~blob-type-check-function
                      black~blob-type-load-function
                      black~blob-type-save-function
                      black~change-blackboard-object-content!
                      black~create-blackboard
                      black~create-blackboard-object
                      black~define-blackboard-object-type
                      black~find-blackboard-object-type
                      black~get-blackboard-object
                      black~get-blackboard-object-content
                      black~get-blackboard-object-type
                      black~new-blackboard-object-type
                      black~read-blackboard-from-file
                      black~remove-blackboard-object!
                      black~write-blackboard-into-file
                      
                      black*blob-types-hashtable))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; It already is an open question whether and how evtl. an environment is shared between different blackboard objects !!    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#| --------------------------------------------------------- global vars ---------------------------------------------------------- |#

(defvar omega*current-blackboard nil)
;; to store the current global blackboard

(defvar black*blob-types-hashtable (make-hash-table))
;; A hash table to store all blackboard-object-descriptions in. The keys are the object-names of the
;; blackboard-object-descriptions

#| ------------------------------------------------------- The blackboard --------------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass black+blackboard (keim+name keim+object)
    ((blackboard-table :initform nil
		       :initarg :blackboard-table
		       :accessor black~blackboard-table))))

(defmethod print-object ((blackboard black+blackboard) stream)
  (format stream "<<<Blackboard ~A>>>" (keim~name blackboard)))
			 
(defun black~create-blackboard (name)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A name.")
	   (effect  "None.")
	   (value   "A new blackboard with a fresh hash-table and name."))
  (make-instance 'black+blackboard
		 :name name
		 :blackboard-table (make-hash-table)))

(defun black~blackboard-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is a blackboard, nil otherwise."))
  (typep obj 'black+blackboard))
  

#| --------------------------------------------------- blackboard object types ----------------------------------------------------- |#

(eval-when (load compile eval)
  (defclass black+blackboard-object-type (keim+name keim+object)
    ((object-check :initform nil
		   :initarg :check-function
		   :accessor black~blob-type-check-function
		   :documentation "A function that checks whether something is of this kind or not.")
     (save-function :initform nil
		    :initarg :save-function
		    :accessor black~blob-type-save-function
		    :documentation "A function that saves the object in a stream. This function should have two argumends: first the object itself and then a stream.")
     (load-function :initform nil
		    :initarg :load-function
		    :accessor black~blob-type-load-function
		    :documentation "A function to load an object. This function should have one arguments: the thing to read itself."))))


(defmethod print-object ((blob-type black+blackboard-object-type) stream)
  (format stream ">>>Blackboard-object-type ~A<<<" (keim~name blob-type)))

(defun black~blackboard-object-type-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is a blackboard-object-type, nil otherwise."))
  (typep obj 'black+blackboard-object-type))

(defun black~new-blackboard-object-type (name check save load)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A name (symbol), the check-function, the save-function and the load function to"
		    "describe a blackboard object type.")
	   (effect  "A new blackboard object type is produced and added to the black*blob-types-hashtable"
		    "with the name as key."
		    "If already a entry with this key exists it is replaced by the new one and a warning is"
		    "produced.")
	   (value   "The new created blackboard-object-type."))

  (let* ((new-blob-type (make-instance 'black+blackboard-object-type
				       :name name
				       :check-function check
				       :save-function save
				       :load-function load))
	 (old-value (gethash name black*blob-types-hashtable)))
    
    (when old-value
      (omega~message "There already exists an entry for ~A in the black*blob-types-hashtable and is replaced now!"
		     name))

    (setf (gethash name black*blob-types-hashtable) new-blob-type)))


(defmacro black~define-blackboard-object-type (&rest attribs)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A written representation of a blackboard-object type (examples see below).")
	   (effect  "Creates the description and enters it into the hashtable black*blob-types-hashtable")
	   (value   "The new blackboard-object type."))
  (do* ((rest-attribs attribs (rest rest-attribs))
	(found-attribs nil)
	(found-name nil)
	(found-check nil)
	(found-save nil)
	(found-load nil))
      ((null rest-attribs)
       (if (and (find 'name found-attribs)
		(find 'check found-attribs)
		(find 'save found-attribs)
		(find 'load found-attribs))
	   `(black~new-blackboard-object-type ',found-name #',found-check #',found-save #',found-load)
	 (omega~error "To specify a blackboard object description one has to specify the parameter name, check, save and load.")))
    
    (let* ((head-attrib (first rest-attribs))
	   (car-attrib (if (null (listp head-attrib))
			   (omega~error "While defining a blackboard object type, the attribute ~A should be a list"
					head-attrib)
			 (first head-attrib)))
	   (second-attrib (second head-attrib)))

      ;; (format t "~%HERE WE ARE")

      (cond ((string-equal car-attrib 'name)
	     
	     (when (find 'name found-attribs)
	       (omega~error "name is already defined during blackboard object type."))

	     (setq found-attribs (cons 'name found-attribs))
	     (setq found-name second-attrib))
	    ((string-equal car-attrib 'check)

	     (when (find 'check found-attribs)
	       (omega~error "check is already defined during blackboard object type."))
	     
	     (setq found-attribs (cons 'check found-attribs))
	     (setq found-check second-attrib))
	    ((string-equal car-attrib 'save)

	     (when (find 'save found-attribs)
	       (omega~error "save is already defined during blackboard object type."))
	     
	     (setq found-attribs (cons 'save found-attribs))
	     (setq found-save second-attrib))
	    ((string-equal car-attrib 'load)

	     (when (find 'load found-attribs)
	       (omega~error "load is already defined during blackboard object type."))
	     
	     (setq found-attribs (cons 'load found-attribs))
	     (setq found-load second-attrib))
	    (t
	     (omega~error "~A is not allowed as atribute for a blackboard object type."
			  head-attrib))))))


#| ---------------------------------------------------- blackboard object --------------------------------------------------------- |#


(eval-when (load compile eval)
  (defclass black+blackboard-object (keim+name keim+object)
    ((content :initform nil
	      :initarg :content
	      :accessor black~blob-content
	      :documentation "The content of the blackboard object.")
     (type :initform nil
	   :initarg :type
	   :accessor black~blob-type
	   :documentation "The blackboard object type of the blackboard object."))))

(defmethod print-object ((blob black+blackboard-object) stream)
  (format stream "<>Blackboard object ~A of type ~A"
	  (keim~name blob) 
	  (black~blob-type blob)))

(defun black~blackboard-object-p (obj)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "Something.")
	   (effect  "None.")
	   (value   "T if input is a blackboard-object, nil otherwise."))
  (typep obj 'black+blackboard-object))

(defgeneric black~create-blackboard-object (name content type)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A name, a content and a type for a blackboard object.")
	   (effect  "None.")
	   (value   "If content mets the chek-function of the blackboard object type"
		    "a new blackboard object is created, otherwise an error is produced."))
  (:method (name content (type symbol))
	   (let* ((type-obj (gethash type black*blob-types-hashtable)))
	     (if type-obj
		 (black~create-blackboard-object name content type-obj)
	       (omega~error "No blackboard object type of name ~A" type))))
  (:method (name content (type black+blackboard-object-type))
	   (let* ((check-function (black~blob-type-check-function type)))
	     (if (apply check-function (list content))
		 (make-instance 'black+blackboard-object
				:name name
				:content content
				:type type)
	       (omega~error "~A is not of blackboard object type ~A"
			    content
			    type)))))

#| ----------------------------------------------------- Other Functions ---------------------------------------------------------- |#

(defgeneric black~add-new-blackboard-object! (name content type blackboard)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A name (symbol), a object, a blackboard object type and a blackboard.")
	   (effect  "First is checked wether the object is allowed to enter in blackboards."
		    "If this is the case a new entry in the hastable of the blackboars is"
		    "done, with key name and value object. If there is already a entry"
		    "with this key a warning is produced but the old value is replaced by the"
		    "new one.")
	   (value   "The entered object."))
  (:method (name content (type symbol) blackboard)
	   (let* ((type-obj (gethash type black*blob-types-hashtable)))
	     (if type-obj
		 (black~add-new-blackboard-object! name content type-obj blackboard)
	       (omega~error "No blackboard object type of name ~A" type))))
  (:method (name content (type black+blackboard-object-type) blackboard)
	   
	   (let* ((blackboard-table (black~blackboard-table blackboard))
		  (new-blackboard-object (black~create-blackboard-object name content type))
		  (old-value (gethash name blackboard-table 'black-no-entry)))
	     
	     (when (null (equal old-value 'black-no-entry))
	       (omega~message "In blackboard ~A there exists already an object with name ~A: ~A. It is replaced by ~A"
			      blackboard name old-value new-blackboard-object))
	     
	     (setf (gethash name blackboard-table) new-blackboard-object)
	     
	     new-blackboard-object)))

(defun black~get-blackboard-object (name blackboard)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A name (symbol) and a blackboard.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: the value of hastable of the blackboard with name as key."
		    "Second: t if such a entry with name as key is in the blackboard at all, nil"
		    "        otherwise."))
  ;; Since nil is returned by default if no entry exists, but also if one exists and is nil in fact, a second value is needed
  ;; for distingushing between the existence and non-existence of an entry

  (let* ((value (gethash name (black~blackboard-table blackboard) 'black-no-entry)))
    (if (equal value 'black-no-entry)
	;; -> no entry
	(values nil nil)
      (values value 't))))

(defun black~get-blackboard-object-content (name blackboard)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A name (symbol) and a blackboard.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "First: the blackboard object content of the value of hastable of the blackboard with name as key."
		    "Second: t if such a entry with name as key is in the blackboard at all, nil"
		    "        otherwise."))
  ;; Since nil is returned by default if no entry exists, but also if one exists and is nil in fact, a second value is needed
  ;; for distingushing between the existence and non-existence of an entry

  (let* ((value (gethash name (black~blackboard-table blackboard) 'black-no-entry)))
    (if (equal value 'black-no-entry)
	;; -> no entry
	(values nil nil)
      (values (black~blob-content value) 't))))


(defun black~get-blackboard-object-type (name blackboard)
  (declare (edited  "18-MAR-1999")
	   (authors Ameier)
	   (input   "A name (symbol) and a blackboard.")
	   (effect  "None.")
	   (value   "Multiple-value:"
		    "The blackboard object type of the blackboard object with name on the blackboard"
		    "if such obne exists, nil otherwise."))
  
  (let* ((value (gethash name (black~blackboard-table blackboard) 'black-no-entry)))
    (if (equal value 'black-no-entry)
	nil
      (black~blob-type value))))


(defun black~remove-blackboard-object! (name blackboard)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A name and a blackboard.")
	   (effect  "The blackboard object entry with the name is removed from the blackboards hashtable.")
	   (value   "Undefined."))
  (remhash name (black~blackboard-table blackboard)))
  

(defun black~change-blackboard-object-content! (name new-content blackboard)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A name, a new blackboard objects content and a blackboard.")
	   (effect  "The content of the blackboard object with name is changes to the new content,"
		    "if the content is consistent with the blackboard objects type, otherwise error.")
	   (value   "The new content."))
  ;; with check, whether the type is still right !!
  (multiple-value-bind
      (blob present)
      (black~get-blackboard-object name blackboard)
    (if (null present)
	(omega~message "No blackboard object with name ~A present.")
      (let* ((blackboard-object-type (black~get-blackboard-object-type name blackboard))
	     (check-function (black~blob-type-check-function blackboard-object-type)))
	(if (apply check-function (list new-content))
	    (setf (black~blob-content blob) new-content)
	  (omega~error "New content ~A is not of blackboard object type ~A" new-content blackboard-object-type))))))


(defun black~find-blackboard-object-type (name)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A name.")
	   (effect  "None.")
	   (value   "Looks up the black*blob-types-hashtable to find a blackboard object type"
		    "with this name and returns this. If no one exists in the hashtable nil is"
		    "returned."))
  (gethash name black*blob-types-hashtable)) 

(defun black~all-blackboard-objects-of-blackboard (blackboard)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A blackboard.")
	   (effect  "None.")
	   (value   "A list of all blackboard-objects of the blackboard."))
  (let* ((values nil))
    (maphash #'(lambda (key value)
		 (setq values (cons value values)))
	     (black~blackboard-table blackboard))
    values))

(defun black~all-blackboard-object-names-of-blackboard (blackboard)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A blackboard.")
	   (effect  "None.")
	   (value   "A list of all names of blackboard-objects of the blackboard."))
  (let* ((names nil))
    (maphash #'(lambda (key value)
		 (setq names (cons key names)))
	     (black~blackboard-table blackboard))
    names))


#| ------------------------------------------------ LOAD + SAVE OF BLACKBOARDS !! --------------------------------------------- |#

(defun black~read-blackboard-from-file (file)
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A file name.")
	   (effect  "None.")
	   (value   "Tries to read the content of the file as blackboard."))
  (with-open-file (stream file :direction :input
			  :if-does-not-exist :error)
		  (let* ((blackboard-input-list (read stream)))
		    (if (and (listp blackboard-input-list)
			     (string= (string (first blackboard-input-list)) "BLACKBOARD"))
			(let* ((name (second blackboard-input-list))
			       (new-blackboard ( black~create-blackboard name))
			       (blackboard-objects (mapcar #'(lambda (blackboard-input)
							       ;; (format t "~% THE BLACKBOARD-INPUT: ~A" blackboard-input)
							       (let* ((blob-name (first blackboard-input))
								      (blob-type-name (second blackboard-input))
								      (blob-type (black~find-blackboard-object-type blob-type-name)))
								 (if (null blob-type)
								     (omega~error "There is no blackboard object type with name ~A"
										  blob-type-name)
								   (let* ((load-function (black~blob-type-load-function blob-type))
									  (blob-content (apply load-function (list
													      (third blackboard-input)
													      ))))
								     
								     (black~add-new-blackboard-object! blob-name
												       blob-content
												       blob-type
												       new-blackboard)))))
							   (rest (rest blackboard-input-list)))))
			  new-blackboard)
		      (error "~%Not able to read the file ~A as a post-problem." file)))))


(defun black~write-blackboard-into-file (blackboard file &key (supersede t))
  (declare (edited  "19-MAR-1999")
	   (authors Ameier)
	   (input   "A blackboard, a file name and a as keyword supersede a flag to sign"
		    "whether a may already existing file with this name should be superseded.")
	   (effect  "Writes the blackboard into the file.")
	   (value   "Undefined."))
  (with-open-file (proof-stream file :direction :output
				:if-exists (if supersede :supersede :append)
				:if-does-not-exist :create)	       
		    (let ((*standard-output* proof-stream))
		      (post~print blackboard *standard-output*))
		    (format *standard-output* ";;; Blackboard ~S written into file ~A"
			    blackboard (merge-pathnames file))))


(defmethod post~print ((blacki black+blackboard) stream)
  (let ((*standard-output* stream))
    (pp~pprint blacki 'blackboard)))

(pp~defstyle blackboard :parent pp+top
	     :help "A style to print blackboards in POST format."
	     :pprint-methods
	     ((black+blackboard
	       (lambda (stream blacki)
		 (let* ((*standard-output* stream)
			(name (keim~name blacki))
			(blobs (black~all-blackboard-objects-of-blackboard blacki)))
		   (pprint-logical-block
		    (nil nil :prefix "(" :suffix ")")
		    (write-string "blackboard ")
		    (write-string (princ-to-string name))
		    (pprint-newline :mandatory)
		    (pprint-logical-block
		     (nil blobs :prefix "" :suffix "")
		     (pprint-exit-if-list-exhausted)
		     (loop
		      (let* ((blob (pprint-pop)))
			(when blob
			  (write blob)
			  (pprint-exit-if-list-exhausted)
			  (pprint-newline :mandatory)))))
		    (pprint-exit-if-list-exhausted)))))
	      (black+blackboard-object
	       (lambda (stream blob)
		 (let*  ((*standard-output* stream)
			 (name (keim~name blob))
			 (type (black~blob-type blob))
			 (type-name (keim~name type))
			 (type-save-function (black~blob-type-save-function type))
			 (content (black~blob-content blob)))
		   (pprint-logical-block
		    (nil nil :prefix "(" :suffix ")")
		    (write-string (princ-to-string name))
		    (write-char #\space)
		    (write-string (princ-to-string type-name))
		    (pprint-newline :mandatory)
		    (apply type-save-function (list content *standard-output*))))))))

