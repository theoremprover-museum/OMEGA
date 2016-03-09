;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-

(in-package "OMEGA")

(defvar ref*ref-hashtable (make-hash-table :test #'equal))

(defclass ref+refinement (keim+name)
  ((input-parameters         :initform nil
		             :accessor ref~input-parameters
			     :initarg :input-parameters)
   (applicability-func       :initform nil
		             :accessor ref~applicability-func
			     :initarg :applicability-func)
   (application-func         :initform nil
		             :accessor ref~application-func
			     :initarg :application-func)
   (application-effect       :initform nil
		             :accessor ref~application-effect
			     :initarg :application-effect)
   (executability-func       :initform nil
		             :accessor ref~executability-func
			     :initarg :executability-func)
   (execution-func           :initform nil
		             :accessor ref~execution-func
			     :initarg :execution-func)
   (execution-effect         :initform nil
		             :accessor ref~execution-effect
			     :initarg :execution-effect)
   ))

(defun ref~refinement-p (obj)
  (or (typep obj 'ref+refinement) (and (strat~p obj) (ref~refinement-p (strat~ref obj)))))

(defmethod print-object ((obj ref+refinement) stream)
  (format stream "Refinement ~A~%with application function ~A" (keim~name obj) (ref~application-func obj)))

(defmacro ref~def-refinement (refname &rest attribs)
  `(block defref
     (let ((input-parameters)
	   (applicability-func)
	   (application-func)
	   (application-effect)
	   (executability-func)
	   (execution-func)
	   (execution-effect))
       (dolist (attrib ',attribs)
	 (cond ((string-equal (first attrib) :input-parameters) (setf input-parameters (second attrib)))
	       ((string-equal (first attrib) :applicability-func) (setf applicability-func (second attrib)))
	       ((string-equal (first attrib) :application-func) (setf application-func (second attrib)))
	       ((string-equal (first attrib) :application-effect) (setf application-effect (second attrib)))
	       ((string-equal (first attrib) :executability-func) (setf executability-func (second attrib)))
	       ((string-equal (first attrib) :execution-func) (setf execution-func (second attrib)))
	       ((string-equal (first attrib) :execution-effect) (setf execution-effect (second attrib)))
	       (t (return-from defref (omega~error ";;;REF~~DEF-REFINEMENT: Not expecting ~A" (car attrib))))))
       (let ((ref (make-instance 'ref+refinement :name ',refname :input-parameters input-parameters :applicability-func applicability-func
				 :application-func application-func :application-effect application-effect
				 :executability-func executability-func :execution-func execution-func :execution-effect execution-effect)))
	 (when (gethash (symbol-name ',refname) ref*ref-hashtable)
	   (omega~warn "Redeclaring refinement ~A" ',refname))
	 (setf (gethash (symbol-name ',refname) ref*ref-hashtable) ref)
	 (omega~message "The following refinement was declared: ~%~A" ref)))))

(defun ref~find-ref (name)
  (let ((meth (meth~find-method name)))
    (if meth
	meth
      (gethash (symbol-name name) ref*ref-hashtable))))

(defun ref~refinement-applicable-p (ref params)
  (if (ref~applicability-func ref)
      (apply (ref~applicability-func ref) params)
    t))

(defun ref~refinement-apply (ref task params)
  (apply (ref~application-func ref) (cons ref (cons task (if (eq params t) nil params)))))

(defun ref~refinement-executable-p (ref task)
  (if (ref~executability-func ref)
      (funcall (ref~executability-func ref) task)
    t))

(defun ref~refinement-execute (ref task params)
  (apply (ref~execution-func ref) (cons task (if (eq params t) nil params))))

(defmacro ref~def-strategic-refinement (refname stratname use-by &optional (control nil))
  (declare (edited  "21-DEZ-1998")
	   (authors Scholl)
	   (input   "A strategy specification: name of the method, list of control rules.")
	   (effect  "Creates an object of the strategy and replaces the method object with it.")
	   (value   "Unspecified."))
  `(block defref
     (let* ((refname ',refname)
	    (ref (if (ref~find-ref refname)
		     (ref~find-ref refname)
		   (omega~error ";;;METH~~DEFSTRATEGY: The method ~A does not exists." methname)))
	    (stratname ',stratname)
	    (use-by (if (and (listp ',use-by) (equal (first ',use-by) 'use-by))
			(second ',use-by)
		      (progn 
			(omega~error ";;;METH~~DEFSTRATEGY: Error in the syntax of the definition of ~A." stratname)
			nil)))
	    (control (if (listp ',control)
			 (if (and (null ',control) (equal use-by :default))
			     (progn
			       (omega~error ";;;METH~~DEFSTRATEGY: The default strategic method must have control rules.")
			       :error)
			   ',control)
		       (let ((control-method (meth~find-method ',control)))
			 (if (and control-method (strat~p control-method))
			     (strat~control control-method)
			   (progn
			     (omega~error ";;;METH~~DEFSTRATEGY: The strategic method ~A, from which the control should be taken, does not exists." control-method)
			     :error))))))
       (when (and method use-by (not (equal control :error)))
	 (omega~message ";;; Defining strategic method ~A as ~A" stratname use-by)
	 (let* ((new-object (make-instance 'strat+strategy
					   :name stratname
					   :ref method
					   :control control)))
	   (setf (gethash (symbol-name stratname) meth*method-hashtable) new-object)
	   (when (equal use-by :default)
	     (meth=replace-method new-object))
	   new-object)))))
