;;;

(in-package "KEIM")

(mod~defmod pdsn
	    :uses (mod node just keim th pdsc pdsj)
	    :documentation "Basic datastructures and functionality of proof plan nodes."
	    :exports (
		      infer~added-pattern-p
		      infer~persistent-pattern-p
		      infer~deleted-pattern-p
		      infer~existent
		      infer~added
		      infer~deleted
		      infer~persistent
		      pdsn~reasons
		      pdsn~normal-supports
		      pdsn~extra-supports
		      pdsn~supports
		      pdsn~add-new-supports
		      pdsn~remove-supports
		      pdsn~failed-steps
		      pdsn~g-inapplicable-methods
		      pdsn~s-inapplicable-methods
		      pdsn~metavars
		      pdsj~justs-equal-at-this-level
		      pds~constraint-store
		      pds~node-formula

		      pds~all-reasons
		      pds~ia-create
		      pds~ie-create
		      pds~pr-create
		      pds~all-reasons
		      pds~remove-more-scopes
		      pds~add-mor-scopes
		      pds~insert-mor-scope
		      pds~set-ancestor-mor-scopes
		      pds~remove-reason-from-history
		      pds~delete-nodes
		      pds~reopen-nodes
		      pds~cstore-bindings
		      pds~mor-goal-p
		      pds~mor-goals
		      pds~mor-nodes
		      pds~update-current-formulas
		      pds~reset-current-formulas
		      pds~adapt-control-infos

		      agenda~insert-at
		      agenda~replace
		      agenda~delete-task-of
		      agenda~task-of
		      agenda~task-control-link
		      agenda~pseudo-goal-nodes
		      agenda~pseudo-goal-step
		      
		      misc~append-to-slot
		      misc~unite-with-slot
		      misc~remove-from-slot

		      subst~create-ntc
		      subst~create-nso
;		      subst~apply-nso
;this is redefined in l-substition anyway
;		      subst~compose-substitution-nso
;		      subst~disjoint-compose-substitution-nso
		      ))

(defmacro misc~append-to-slot (slot extension)
  `(if (listp ,extension)
       (setf ,slot (append ,slot ,extension))
     (setf ,slot (append ,slot (list ,extension)))))

(defmacro misc~unite-with-slot (slot extension)
  `(if (listp ,extension)
       (setf ,slot (union ,slot ,extension))
     (setf ,slot (union ,slot (list ,extension)))))

(defmacro misc~remove-from-slot (slot deletes)
  `(if (listp ,deletes)
       (setf ,slot (set-difference ,slot ,deletes))
     (setf ,slot (set-difference ,slot (list ,deletes)))))

;;; Aufpassen beim Erzeugen von neuen Justifications: bereits existierende control info muss vererbt werden:

;;; pdsn~reasons
;; sollte accessor of pdsn+node sein!
(defun pdsn~reasons (node)
  (pdsj~reasons (node~justification node)))

(defsetf pdsn~reasons (node) (reasons)
  `(setf (pdsj~reasons (node~justification ,node)) ,reasons))


;;; pdsn~failed-steps
;;; pdsn~permanently-inapplicable-methods
;;; pdsn~instantly-inapplicable-methods
;; sollten accessors of pdsn+node sein!
;; failed-steps : failed-methods
;; g-inapplicable-methods : alternative-methods
;; s-inapplicable-methods : alternative-mmatchings
(defun pdsn~failed-steps (node)
  (pdsn~failed-methods node))

(defsetf pdsn~failed-steps (node) (steps)
  `(setf (pdsn~failed-methods ,node) ,steps))

(defun pdsn~g-inapplicable-methods (node)
  (pdsn~alternative-methods node))

(defsetf pdsn~g-inapplicable-methods (node) (methods)
  `(setf (pdsn~alternative-methods ,node) ,methods))

(defun pdsn~s-inapplicable-methods (node)
  (pdsn~alternative-mmatchings node))

(defsetf pdsn~s-inapplicable-methods (node) (methods)
  `(setf (pdsn~alternative-mmatchings ,node) ,methods))


;;; pdsn~supports
;; sollte accessor of pdsn+node sein!
(defun pdsn~normal-supports (node)
  (pdsn~just-sponsors node))

(defsetf pdsn~normal-supports (node) (nodes)
  `(setf (pdsn~just-sponsors ,node) ,nodes))

(defun pdsn~extra-supports (node)
  (pdsn~just-unsponsors node))

(defsetf pdsn~extra-supports (node) (nodes)
  `(setf (pdsn~just-unsponsors ,node) ,nodes))

(defun pdsn~supports (node)
  (union (pdsn~normal-supports node) (pdsn~extra-supports node)))

(defun pdsn~add-new-supports (goal new-supports &optional pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A goal node and list of new support nodes.")
	   (effect  "Updates the support slots of the goal node.")
	   (value   "Undefined."))
  ;; optional pds argument is for outline-action calls
  (if (pdsn~s-inapplicable-methods goal)
      (misc~unite-with-slot (pdsn~extra-supports goal) new-supports)
    (misc~unite-with-slot (pdsn~normal-supports goal) new-supports)))

(defun pdsn~remove-supports (goal supports &optional pds)
  (declare (edited  "06-JUN-2000")
	   (authors Scholl)
	   (input   "A goal node and list of support nodes.")
	   (effect  "Removes the supports from the both the support and the extra support slot.")
	   (value   "Undefined."))
  ;; optional pds argument is for outline-action calls
  (misc~remove-from-slot (pdsn~normal-supports goal) supports)
  (misc~remove-from-slot (pdsn~extra-supports goal) supports))

;;; Umbenennung
(defun pds~constraint-store (pds)
  (pds~constraint-pool pds))

(defsetf pds~constraint-store (pds) (cstore)
  `(setf (pds~constraint-pool ,pds) ,cstore))

;;; Patterns for outlines: Erweitern und Ueberdenken mit Volker und Andreas
;; "EXISTENT" gibt es schon als Konstante infer*existent
(defconstant infer*added "ADDED")
(defconstant infer*persistent "PERSISTENT")
(defconstant infer*deleted "DELETED")

(defun infer~added-pattern-p (pattern)
  (string-equal pattern infer*added))

(defun infer~persistent-pattern-p (pattern)
  (string-equal pattern infer*persistent))

(defun infer~deleted-pattern-p (pattern)
  (string-equal pattern infer*deleted))

(defun infer~existent ()
  infer*existent)

(defun infer~deleted ()
  infer*deleted)

(defun infer~added ()
  infer*added)

(defun infer~persistent ()
  infer*persistent)

;;; agenda~insert-at
(defun agenda~insert-at (tasks orderings task agenda)
  (agenda~insert-tasks task nil tasks orderings agenda))

(defun agenda~replace (task tasks orderings agenda)
  (agenda~replace-task task nil tasks orderings agenda))

(defun agenda~delete-task-of (node agenda)
  (let ((task (agenda~get-first-task agenda #'(lambda (tk)
						(eq (agenda~task-node tk) node)))))
    (if task
	(agenda~replace task nil nil agenda)
      agenda)))

(defun agenda~task-of (node agenda)
  (agenda~get-first-task agenda #'(lambda (tk)
				    (eq (agenda~task-node tk) node))))



(in-package "OMEGA")

;;; meth~check-condition
;; - sollte in mmapp kein constraint store bzw. constraint state, sondern constraint zurueckgeben
;; - auch als Eingabe eine Bindung von Meta-variablen (wird gebraucht bei der Expansion)
;; - neu erzeugten Meta-variablen zurueckliefern


;;; meth~carry-out-computations
;; (- sollte in mmapp kein constraint store bzw. constraint state, sondern constraint zurueckgeben)
;; - als Eingabe eine Bindung von Meta-variablen 
;; - neu erzeugten Meta-variablen zurueckliefern zusaetzlich zu den alten Rueckgabewerten

;;; meth~complete-outlines
;; - als Eingabe eine Bindung von Meta-variablen 
;; - neu erzeugten Meta-variablen zurueckliefern zusaetzlich zu den alten Rueckgabewerten

;;; cstr~merge
;; - Eingabe: constraint (was man aus meth~check-condition bekommt), Liste von den relevanten Infos aus
;; MOR-scopes, z.B. constraint states
;; - Rueckgabe: new constraint state, domain, neue Bindungen (Informationen, die man fue die Weiterbearbeitung
;; brauchrt)

;;; cstr~related-to-p
;; - Eingabe: constraint, MOR scope
;; - Rueckgabe: T, falls ...

;(defun cri~call (set &key ((:kind kind)) ((:rules rules)) ((:mode mode)))
;  set)

;(setq plan*trace t)
;(setq back*trace t)

