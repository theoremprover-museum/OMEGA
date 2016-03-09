(in-package "OMEGA")

(defvar cri*parameter-control-rules-hashtable (make-hash-table :test #'equal))
(defvar cri*support-control-rules-hashtable (make-hash-table :test #'equal))
(defvar cri*setting-control-rules-hashtable (make-hash-table :test #'equal))

(defmacro cri~def-parameter-control-rule (name for-method if-part to-do-part)
  `(if (and (consp ',for-method) (equal (first ',for-method) 'for))
       (let ((method (second ',for-method)))
	 (if (ref~find-ref method)
	     (let ((crule (cri~def-control-rule ,name (kind parameters) ,if-part ,to-do-part))
		   (crules (gethash method cri*parameter-control-rules-hashtable)))
	       (if (find ',name crules)
		   (progn
		     (omega~warn "Control rule already exists and will be replaced.")
		     (setf (gethash method cri*parameter-control-rules-hashtable)
			   (substitute-if (keim~name crule) #'(lambda (cr) (keim~equal cr ',name)) crules)))
		 (setf (gethash method cri*parameter-control-rules-hashtable)
		       (append crules (list (keim~name crule))))))
	   (omega~error "Method specified in control rule ~A does not exist: ~A" ',name method)))
     (omega~error "Error in control rule syntax in rule ~A" ',name)))

(defun cri~parameter-control-rules-for (method)
  (cond ((or (ref~refinement-p method) (meth~p method)) (cri~parameter-control-rules-for (keim~name method)))
	(t (gethash method cri*parameter-control-rules-hashtable))))

(defmacro cri~def-support-control-rule (name for-method if-part to-do-part)
  `(if (and (consp ',for-method) (equal (first ',for-method) 'for))
       (let ((method (second ',for-method)))
	 (if (ref~find-ref method)
	     (let ((crule (cri~def-control-rule ,name (kind supports) ,if-part ,to-do-part))
		   (crules (gethash method cri*support-control-rules-hashtable)))
	       (if (find ',name crules)
		   (progn
		     (omega~warn "Control rule already exists and will be replaced.")
		     (setf (gethash method cri*support-control-rules-hashtable)
			   (substitute-if (keim~name crule) #'(lambda (cr) (keim~equal cr ',name)) crules)))
		 (setf (gethash method cri*support-control-rules-hashtable)
		       (append crules (list (keim~name crule))))))
	   (omega~error "Method specified in control rule ~A does not exist: ~A" ',name method)))
     (omega~error "Error in control rule syntax in rule ~A" ',name)))

(defun cri~support-control-rules-for (method)
  (cond ((meth~p method) (cri~support-control-rules-for (keim~name method)))
	(t (gethash method cri*support-control-rules-hashtable))))

(defmacro cri~def-setting-control-rule (name for-method if-part to-do-part)
  `(if (and (consp ',for-method) (equal (first ',for-method) 'for))
       (let ((method (second ',for-method)))
	 (if (ref~find-ref method)
	     (let ((crule (cri~def-control-rule ,name (kind settings) ,if-part ,to-do-part))
		   (crules (gethash method cri*setting-control-rules-hashtable)))
	       (if (find ',name 'keim~name crules)
		   (progn
		     (omega~warn "Control rule already exists and will be replaced.")
		     (setf (gethash method cri*setting-control-rules-hashtable)
			   (substitute-if (keim~name crule) #'(lambda (cr) (keim~equal cr ',name)) crules)))
		 (setf (gethash method cri*setting-control-rules-hashtable)
		       (append crules (list (keim~name crule))))))
	   (omega~error "Method specified in control rule ~A does not exist: ~A" ',name method)))
     (omega~error "Error in control rule syntax in rule ~A" ',name)))

(defun cri~setting-control-rules-for (method)
  (cond ((meth~p method) (cri~setting-control-rules-for (keim~name method)))
	(t (gethash method cri*setting-control-rules-hashtable))))

(defun cri~n-tuples-from-sequence (n supps)
  (let ((m (length supps)))
    (cond ((< m n) nil)
	  ((< n 1) (list nil))
	  (( = n 1) (mapcar 'list supps))
	  (t (append (cri~insert-at-every-position (first supps) (cri~n-tuples-from-sequence (- n 1) (rest supps)))
		     (cri~n-tuples-from-sequence n (rest supps)))))))

(defun cri~insert-at-every-position (supp supp-tuples)
  (when supp-tuples
    (append (cri~insert-at-every-position-in-tuple supp (first supp-tuples))
	    (cri~insert-at-every-position supp (rest supp-tuples)))))

(defun cri~insert-at-every-position-in-tuple (supp supp-tuple)
  (loop for i from 0 to (length supp-tuple)
	collect (cri~insert-at i supp supp-tuple)))

(defun cri~insert-at (i supp supp-tuple)
  (append (subseq supp-tuple 0 i)
	  (list supp)
	  (subseq supp-tuple i (length supp-tuple))))
