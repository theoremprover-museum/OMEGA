
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functions for the NIC calculus of Byrnes/Sieg
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :omega)

(defvar nic*substitution (subst~create nil nil)
  "The global substitution to be mantained for the NIC calculus")

(defvar nic*substitution-hashtable (make-hash-table :test #'equal)
  "The substitution hashtable for backtracking to be mantained for the NIC calculus. This hashtable is
indexed by pairs of timestamps and labels of proof nodes in the pds.")

(defun nic~substitution-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The substitution hashtable."))
  nic*substitution-hashtable)

(defun nic~initialiaze-substitution-hashtable ()
  (setf nic*substitution-hashtable (make-hash-table :test #'equal))
  (setf nic*substitution (subst~create nil nil))
  (nic~print-substitution-hashtable-info))


(defun nic~substitution-hashtable2list ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The substitution hashtable represented as a list."))
   (let ((index-subst-pairs nil))   ;;; das hier muss doch irgendwie effizienter gehen?
     (maphash #'(lambda (x y) (push (list x y) index-subst-pairs))
	      (nic~substitution-hashtable))
     index-subst-pairs))

(defun nic~print-substitution-hashtable-info ()
  (omega~message "Current substution-hashtable: ~A" (nic~substitution-hashtable2list))
  (omega~message "Represented substitution: ~A" nic*substitution))    


;; Just for the demo with Andrew Ireland, I will not use this here because of the write/read-pds failure
;
;(defmethod node~formula :around ((node pdsn+node))
;  (let ((formula (call-next-method)))
;    (if nic*substitution
;        (prog1 (subst~apply nic*substitution formula))
;      formula)))

(defun nic~index (node)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "a node (or a name of node)")
	   (effect  "None")
	   (value   "an index entry used for indexeing the substitution-hashtable"))
  (let ((name (if (stringp node) node (keim~name node))))
    (list (get-universal-time) (string-upcase name))))

(defun nic~index-stamp-= (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (= (first index1) (first index2)))

(defun nic~index-stamp-< (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (< (first index1) (first index2)))

(defun nic~index-stamp-<= (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (<= (first index1) (first index2)))


(defun nic~index-node-eq (index1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   )
	   (effect  )
	   (value   ))
  (equal (second index1) (second index2))) 
    

(defun nic~index-node-eq-special (node1 index2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "A node (or a node-name) and a index entry")
	   (effect  "None")
	   (value   "T iff node1 and the node-part of the index-entry are the same"))
  (let ((node-name (if (node~p node1) (keim~name node1) node1)))
    (string-equal node-name (second index2))))


(defun nic~get-substs-for-node (node)
  (declare (edited  "24-MAY-2000")
	   (authors Ceb)
	   (input   "A node")
	   (effect  "None")
	   (value   "The list of all substitutions for node in the substitution hashtable"))
  (let* ((mapping-indices
	  (remove-if #'nic~backtracked-entry-p 
		     (remove-if-not #'(lambda (x)
					(nic~index-node-eq-special node x))
				    (nic~suggestion-hashtable-indices))))
	 (corresp-entries (mapcar #'(lambda (x)
				      (gethash x (nic~substitution-hashtable)))
				   mapping-indices)))
    corresp-entries))

(defun nic~backtracked-entry-p (entry)
  (equal 'backtracked (second entry)))

(defun nic~add-subst (subst node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "A substitution and a node ")
	   (effect  "If subst is consistent with the substitution hashtable, i.e., the"
		    "the global substitution, then subst is appended to them."
		    "Furthermore the value the nic*substitution is updated")
	   (value   "The updated value of  nic*substitution, representing the idempotent"
		    "substitution described by the hashtable"))
  (when (and (nic~subst-consistent-p nic*substitution subst)
	     (<= (length (nic~get-substs-for-node node)) 1))
    (setf (gethash (nic~index node) (nic~substitution-hashtable)) subst)
    (setf nic*substitution (nic~compute-subst-represented-by-hashtable))
    (nic~print-substitution-hashtable-info)
    (nic~substitution-hashtable)))
	    

(defun nic~del-subst (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "a node")
	   (effect  "Deletes the substitution stored for node in the substitution hashtable together"
		    "with all other substitutions having a timestamp greater than that associated"
		    "with node.")
	   (value   "The updated value of  nic*substitution, representing the idempotent"
		    "substitution described by the hashtable"))
  (let* ((index-for-node (find-if #'(lambda (x) (nic~index-node-eq-special node x)) (nic~suggestion-hashtable-indices)))
	 (indices-to-del (when index-for-node
			   (remove-if #'(lambda (x) (nic~index-stamp-< x index-for-node))
				      (nic~suggestion-hashtable-indices)))))
    (print index-for-node)
    (print indices-to-del)
    (dolist (index indices-to-del nil)
      (setf (gethash index (nic~substitution-hashtable)) 'backtracked))
    (setf nic*substitution (nic~compute-subst-represented-by-hashtable))
    (nic~print-substitution-hashtable-info) ;;; for debugging
    (nic~substitution-hashtable)))


(defun nic~suggestion-hashtable-indices ()
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "None")
	   (effect  "None.")
	   (value   "All indices of the substitution hashtable."))
  (let ((list nil))
    (maphash #'(lambda (x y) (declare (ignore y)) (push x list)) (nic~substitution-hashtable))
    list))


(defun nic~suggestion-hashtable-entries ()
  (declare (edited  "21-NOV-1999")
	   (authors Sorge)
	   (input   "None")
	   (effect  "None.")
	   (value   "All entries of the substitution hashtable."))
    (let ((list nil))
      (maphash #'(lambda (x y) (declare (ignore x)) (push y list)) (nic~substitution-hashtable))
      list))


(defun  nic~subst-consistent-p (subst1 subst2)
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "Two substitutions")
	   (effect  "None")
	   (value   "T iff both substitutions are compatible."))
  (let* ((idem-subst1 (subst~idem subst1))
	 (idem-subst2 (subst~idem subst2))
	 (dom1 (subst~domain idem-subst1))
	 (dom2 (subst~domain idem-subst2)))
    (setf testvar1 (car dom1))
    (setf testvar2 (car dom2))
    (not (intersection dom1 dom2 :test #'term~alpha-equal))))


(defun nic~compute-subst-represented-by-hashtable ()
  (declare (edited  "22-MAY-2000")
	   (authors Ceb)
	   (input   "None")
	   (effect  "None")
	   (value   "The idempotent substitution represented by the substitution hashtable"))
  (let* ((index-subst-pairs
	  (remove-if #'nic~backtracked-entry-p (nic~substitution-hashtable2list)))
	 (sorted-index-subst-pairs (sort index-subst-pairs
					#'(lambda (pair1 pair2)
					    (nic~index-stamp-< (first pair1)
							       (first pair2)))))
	(subst (subst~create nil nil)))
    (mapc #'(lambda (pair)
	      (setf subst (subst~compose-substitution subst (second pair))))
	  sorted-index-subst-pairs)
    (setf subst (subst~idem subst))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;       NIC open node classifier                          ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar nic*classifier-hashtable (make-hash-table :test #'equal)
  "Hash table, indexed by rule/tactic name, that holds classifiers for the outlines of all NIC rules/tactics.")


(defun nic~classifier-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The classifier hashtable."))
  nic*classifier-hashtable)

(defun nic~set-up-nic-classifier-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None")
	   (effect  "Sets up the NIC classifier hashtable. The entries specify for each"
		    "node of the outline of a NIC rule/tactic whether this particular"
		    "node is an :e (elimination, major premise), :i (introduction), or"
		    ":n (indifferent, i.e. both) node.")
	   (value   "The  NIC classifier hashtable."))
  (setf (gethash (string-upcase "NIC-IMP-I") (nic~classifier-hashtable))        (list :I :N))
  (setf (gethash (string-upcase "NIC-AND-I") (nic~classifier-hashtable))        (list :I :N :N))
  (setf (gethash (string-upcase "NIC-FORALL-I") (nic~classifier-hashtable))     (list :I :N))
  (setf (gethash (string-upcase "NIC-NEG-I") (nic~classifier-hashtable))        (list :I :N))
  (setf (gethash (string-upcase "NIC-FALSE-C") (nic~classifier-hashtable))      (list :I :N))
  (setf (gethash (string-upcase "NIC-EXISTS-I") (nic~classifier-hashtable))     (list :I :N))
  (setf (gethash (string-upcase "NIC-EXISTS-E-i") (nic~classifier-hashtable))   (list :I :E :N))
  (setf (gethash (string-upcase "NIC-OR-I-l") (nic~classifier-hashtable))       (list :I :N))
  (setf (gethash (string-upcase "NIC-OR-I-r") (nic~classifier-hashtable))       (list :I :N))
  (setf (gethash (string-upcase "NIC-OR-E-i") (nic~classifier-hashtable))       (list :I :E :N :N))
  (setf (gethash (string-upcase "NIC-IMP-E") (nic~classifier-hashtable))        (list :N :E :N))
  (setf (gethash (string-upcase "NIC-AND-E-l") (nic~classifier-hashtable))      (list :N :E))
  (setf (gethash (string-upcase "NIC-AND-E-r") (nic~classifier-hashtable))      (list :N :E))
  (setf (gethash (string-upcase "NIC-FORALL-E") (nic~classifier-hashtable))     (list :N :E))
  (setf (gethash (string-upcase "NIC-NEG-E") (nic~classifier-hashtable))        (list :N :E :N))
  (setf (gethash (string-upcase "NIC-EXISTS-E-e") (nic~classifier-hashtable))   (list :N :E :E))
  (setf (gethash (string-upcase "NIC-OR-E-e") (nic~classifier-hashtable))       (list :N :E :E :E))
  (setf (gethash (string-upcase "NICTAC-IMP-E") (nic~classifier-hashtable))     (list :N :E :N))
  (setf (gethash (string-upcase "NICTAC-AND-E-L") (nic~classifier-hashtable))   (list :N :E))
  (setf (gethash (string-upcase "NICTAC-AND-E-R") (nic~classifier-hashtable))   (list :N :E))
  (setf (gethash (string-upcase "NICTAC-NEG-E") (nic~classifier-hashtable))     (list :N :E :N))
  (setf (gethash (string-upcase "NICTAC-OR-E-e") (nic~classifier-hashtable))    (list :N :E :E :E))
  (setf (gethash (string-upcase "NICTAC-OR-E-i") (nic~classifier-hashtable))    (list :I :E :N :N))
  (setf (gethash (string-upcase "NICTAC-OR-E") (nic~classifier-hashtable))      (list :I :E :N :N))
;;;;;;;; Additional Tactics extending NIC
  (setf (gethash (string-upcase "NICTAC-EQUIV-I") (nic~classifier-hashtable))      (list :I :N :N))
  (setf (gethash (string-upcase "NICTAC-MODTOLL") (nic~classifier-hashtable))    (list :N :E :N))
  (setf (gethash (string-upcase "SOLVED-OR-RESULT-BY-LEO") (nic~classifier-hashtable))    (list :N))
  (setf (gethash (string-upcase "SOLVED-BY-LEO-PL-ATP") (nic~classifier-hashtable))    (list :N))
  (setf (gethash (string-upcase "SOLVED-BY-FO-ATP") (nic~classifier-hashtable)) (list :N))
  (setf (gethash (string-upcase "COUNTEREXAMPLE-BY-SATCHMO") (nic~classifier-hashtable)) (list :N))
  (setf (gethash (string-upcase "SET-EXT-EXPAND") (nic~classifier-hashtable))   (list :N :E))
  (setf (gethash (string-upcase "SET-EXT-CONTRACT") (nic~classifier-hashtable)) (list :I :N))
  (setf (gethash (string-upcase "SET-EXT-CONTRACT*") (nic~classifier-hashtable)) (list :I :N))
;;;  (setf (gethash (string-upcase "CNF-NORMALIZE") (nic~classifier-hashtable)) (list :I :N))
  (setf (gethash (string-upcase "PRENEX-FORM") (nic~classifier-hashtable)) (list :I :N))
  (setf (gethash (string-upcase "DEFN-EXPAND*") (nic~classifier-hashtable))     (list :N :E))
  (setf (gethash (string-upcase "DEFNE*") (nic~classifier-hashtable))           (list :N :E))
  (setf (gethash (string-upcase "DEFN-CONTRACT*") (nic~classifier-hashtable))    (list :I :N))
  (setf (gethash (string-upcase "DEFNI*") (nic~classifier-hashtable))    (list :I :N))
  (setf (gethash (string-upcase "OTTER") (nic~classifier-hashtable))    (list :N))
  (setf (gethash (string-upcase "LEO-DERIVATION") (nic~classifier-hashtable)) (list :I :N))
  (setf (gethash (string-upcase "ANDI*") (nic~classifier-hashtable))    (list :I :N))
  (setf (gethash (string-upcase "INDIRECT") (nic~classifier-hashtable))    (list :I :N))
  (setf (gethash (string-upcase "NOTE") (nic~classifier-hashtable))     (list :N :E :N))
  (setf (gethash (string-upcase "SIMPLIFY-WITH-CAS") (nic~classifier-hashtable)) (list :N :N))
  (setf (gethash (string-upcase "THATE") (nic~classifier-hashtable)) (list :N :N :N))
  (setf (gethash (string-upcase "THATI") (nic~classifier-hashtable)) (list :I :N :N))
  (omega~message "Setting up the classifier hashtable for NIC rules and tactics.")
  (nic~classifier-hashtable))

;;;; Set up hashtable, when loading this file

(nic~set-up-nic-classifier-hashtable)

(defun nic~lookup-classifier (obj)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "A string or a symbol.")
	   (effect  "None.")
	   (value   "The entry in the rule/tactic classifier hashtable associated with
obj"))
  (if (stringp obj) 
      (gethash  (string-upcase obj) (nic~classifier-hashtable))
    (when (symbolp obj) (gethash (string-upcase (symbol-name obj)) (nic~classifier-hashtable)))))

(defgeneric nic~e-i-classify-proof-context (obj)
  (declare (edited  "29-12-99")
           (authors Chris)
           (input   "A proof context")
           (effect  "None")
           (value   "The modified proof context."))
  (:method ((obj foci+pc))
	   (let ((descendants (foci~descendants (foci~active-pc))))
	     (if descendants (nic~e-i-classifier (foci~focus-line (foci~active-pc))
						 (first descendants))
	       :N)))
  (:method (obj)
	   (omega~error "~A should be a proof context." obj)))

(defun nic~e-i-classifier (focus-line father-node)
  (declare (edited  "07-JAN-2000")
	   (authors Chris)
	   (input   "A node and its father node.")
	   (effect  "None.")
	   (value   "None, the NIC e-i-classifier."))
  (let* ((father-rule-name (keim~name (pdsn~just-method father-node)))
	 (father-rule-classifier (nic~lookup-classifier father-rule-name))
	 (father-outline (cons father-node (pdsn~just-premises father-node))))
    (if (= (length father-outline) (length father-rule-classifier))
	(some #'(lambda (x y) (when (equal (keim~name focus-line)
					   (keim~name x))
				Y))
	      father-outline
	      father-rule-classifier)
      (omega~message "Inconsistency in nic-classifier-hashtable: ~A ~A ~A" father-rule-name father-rule-classifier father-outline)))
  :N)
  
(defun nic~elimination-p (&optional node)
  (declare (edited  "07-JAN-2000")
	   (authors Chris)
	   (input   "Optinally a node.")
	   (effect  "None.")
	   (value   "T iff the current proof context (or optionally the node) is an"
		    "elimination context, i.e. only elimination rules are applicable."))
  (let* ((reasons (when node (pdsj~reasons (node~justification node))))
	 (node-classifier (if node
			      (if reasons
				  (nic~e-i-classifier node (pdsc~an-node (first (last
										 reasons))))
				:N)
			    (nic~e-i-classify-proof-context (foci~active-pc)))))
      (equal :E node-classifier)))
      


(defun nic~introduction-p (&optional node)
  (declare (edited  "07-JAN-2000")
	   (authors Chris)
	   (input   "Optinally a node.")
	   (effect  "None.")
	   (value   "T iff the current proof context (or optionally the node) is an"
		    "introduction context, i.e. only introduction rules are applicable."))
  (let* ((reasons (when node (pdsj~reasons (node~justification node))))
	 (node-classifier (if node
			      (if reasons
				  (nic~e-i-classifier node (pdsc~an-node (first (last
										 reasons))))
				:N)
			    (nic~e-i-classify-proof-context (foci~active-pc)))))
      (or (equal :I node-classifier)
	  (equal :N node-classifier))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;       NIC OR-E-INFO HASHTABLE                           ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar nic*or-e-info-hashtable (make-hash-table :test #'equal)
  "Hash table, indexed by node name, that holds nic-or-elimination information.")


(defun nic~or-e-info-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The nic-or-elimination information hashtable."))
  nic*or-e-info-hashtable)

(defvar nic*or-e-removed-info-hashtable (make-hash-table :test #'equal)
  "Hash table, indexed by node name, that holds nic-or-elimination removed information.")


(defun nic~or-e-removed-info-hashtable ()
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "None.")
	   (effect  "None.")
	   (value   "The nic-or-elimination removed information hashtable."))
  nic*or-e-removed-info-hashtable)

(defun nic~lookup-or-e-info (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "The current open node in proof search")
	   (effect  "None.")
	   (value   "Looks up the entry for node in the nic-or-elimination information hashtable."))
  (gethash (string-upcase (keim~name node)) (nic~or-e-info-hashtable)))

(defun nic~lookup-or-e-removed-info (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "The current open node in proof search")
	   (effect  "None.")
	   (value   "Looks up the entry for node in the nic-or-elimination removed information hashtable."))
  (gethash (string-upcase (keim~name node)) (nic~or-e-removed-info-hashtable)))

(defun nic~remove-or-e-info (node entry)
  (declare (edited  "11-JAN-2000" "06-JAN-2000")
	   (authors Sorge Chris)
	   (input   "A node and an entry.")
	   (effect  "Removes the entry for node in the nic-or-elimination information hashtable.")
	   (value   "Undefined."))
  (let* ((node-name (string-upcase (keim~name node)))
	 (old-entry (gethash node-name (nic~or-e-info-hashtable)))
	 (new-entry (remove entry old-entry :test #'nic=or-e-info-equal)))
    (setf (gethash node-name (nic~or-e-info-hashtable)) new-entry)
    (when (term~p entry)
      (setf (gethash node-name (nic~or-e-removed-info-hashtable))
	    (cons entry (gethash node-name (nic~or-e-removed-info-hashtable)))))))

(defun nic=or-e-info-equal (x y)
  (cond ((and (term~p x) (term~p y))
	 (term~alpha-equal x y))
	((and (listp x) (listp y))
	 (and (term~alpha-equal (first x) (first y))
	      (term~alpha-equal (second x) (second y))))))

(defun nic~add-or-e-info (node disj term)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "The current open node in proof search, the disjunction term, and"
		    "the left or right disjunct")
	   (effect  "Adds an entry to the nic-or-elimination information hashtable.")
	   (value   "Undefined."))
  (when (and (logic~disjunction-p disj)
	     (or (keim~equal term (first (data~appl-arguments disj)))
		 (keim~equal term (second (data~appl-arguments disj))))
	     (not (find disj (nic~lookup-or-e-removed-info node) :test #'nic=or-e-info-equal)))
;;    (format t "Node ~A  term ~A  removed ~{~A ~}  info ~{~A ~}  test ~A~%"
;;            node
;;            disj
;;            (nic~lookup-or-e-removed-info node)
;;            (nic~lookup-or-e-info node)
;;            (not (find disj (nic~lookup-or-e-removed-info node) :test #'nic=or-e-info-equal)))
    (let* ((entries (reverse (copy-list (nic~lookup-or-e-info node))))
	   (new-entries (let ((partner (find-if #'(lambda (x)
						    (and (listp x)
							 (keim~equal (first x) disj)
							 (not (keim~equal (second x) term))))
						entries)))
			  
			  (if partner (let ((new-list (remove-if #'(lambda (x)
								     (and (listp x)
									  (keim~equal (first x) disj)))
								 entries)))
					(pushnew disj new-list :test #'nic=or-e-info-equal))
			    (pushnew (list disj term) entries :test #'nic=or-e-info-equal)))))
      (setf (gethash (string-upcase (keim~name node)) (nic~or-e-info-hashtable)) (reverse new-entries)))))

(defun nic~lookup-or-e-applicable-disjunctions (node)
  (declare (edited  "06-JAN-2000")
	   (authors Chris)
	   (input   "A node.")
	   (effect  "None.")
	   (value   "Looks up the disjunction candidates for or-e."))
  (let ((entries (nic~lookup-or-e-info node)))
    (mapcan #'(lambda (x) (when (and (term~p x) (logic~disjunction-p x))
			    (list x)))
	    entries)))


(defvar nic*command-list
  '(NICTAC-WEAKEN      ;; original NIC
    NICTAC-FOWEAKEN    ;; original NIC
    NIC-FORALL-I       ;; original NIC
    NIC-EXISTS-E-I     ;; original NIC
    NIC-EXISTS-E-E     ;; original NIC
    NIC-FORALL-E       ;; original NIC
    NIC-EXISTS-I       ;; original NIC
    SET-EXT-EXPAND     ;; additional stuff to deal set equations and extensionality
    SET-EXT-CONTRACT   ;; additional stuff to deal set equations and extensionality
    SET-EXT-CONTRACT*   ;; additional stuff to deal set equations and extensionality
;;    CNF-NORMALIZE   ;; additional stuff to deal with cnf normalization
    PRENEX-FORM     ;; prenex normalisation
    DEFN-CONTRACT*     ;; additional stuff to deal with definitions
    NICTAC-EQUIV-I     ;; equivalence introduction
    NIC-IMP-I          ;; original NIC
    NIC-AND-I          ;; original NIC
    NIC-OR-I-L         ;; original NIC
    NIC-OR-I-R         ;; original NIC
    NIC-NEG-I          ;; original NIC
    THAT-I             ;; dealing with the description operator backward
    THAT-E             ;; dealing with the description operator forward
    NICDUMMY-OR-E-R    ;; original NIC
    NICDUMMY-OR-E-L    ;; original NIC
    NICTAC-AND-E-R     ;; original NIC
    NICTAC-AND-E-L     ;; original NIC
    NICTAC-IMP-E       ;; original NIC
    NICTAC-MODTOLL     ;; modus tollens
    NICTAC-NEG-E       ;; original NIC
    NICTAC-OR-E        ;; original NIC
    DEFN-EXPAND*       ;; additional stuff to deal with definitions
    NIC-FALSE-C        ;; original NIC
    )
  "A list of Nic commands sorted in the order of best applicability.")
  

(defmethod nic~use-nic-agents ()
  (csm~set-considered-commands nic*command-list))

(defun nic~use-nic-heuristics ()
  (csm~set-considered-classifiers nil)
  (auto~set-criteria)
  (agent~init-resources))
  
(defun nic~use-external-agents ()
  (csm~set-considered-commands
   '(SOLVED-BY-FO-ATP
     SOLVED-OR-RESULT-BY-LEO
     COUNTEREXAMPLE-BY-SATCHMO
     SOLVED-BY-PL-ATP
     SOLVED-BY-LEO-PL-ATP
     SIMPLIFY-GOAL-WITH-CAS
     SOLVED-BY-TPS
     )))

(defun nic~add-external-agents ()
  (csm~set-considered-commands
   (append
    '(SOLVED-BY-FO-ATP
     SOLVED-OR-RESULT-BY-LEO
     COUNTEREXAMPLE-BY-SATCHMO
     SOLVED-BY-PL-ATP
     SOLVED-BY-LEO-PL-ATP
     SIMPLIFY-GOAL-WITH-CAS
     )
    (csm~considered-commands))))

(defgeneric nic~add-command-agents (agents)
  (:method ((agents cons))
	   (csm~set-considered-commands (append agents (csm~considered-commands))))
  (:method (agent)
	   (csm~set-considered-commands (cons agent (csm~considered-commands)))))


;;;; Some stuff for the theories.

(defun nic~universal-quantification-p (term)
  (and
   (logic~universal-quantification-p term)
   (data~schema-equal (data~appl-function term)
		      (env~lookup-object :forall (pds~environment omega*current-proof-plan)))))

(defun nic~existential-quantification-p (term)
  (and
   (logic~existential-quantification-p term)
   (data~schema-equal (data~appl-function term)
		      (env~lookup-object :exists (pds~environment omega*current-proof-plan)))))

