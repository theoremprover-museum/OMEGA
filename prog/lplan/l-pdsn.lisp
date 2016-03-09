;;; -*- syntax: common-lisp; package: keim; base: 10; mode: keim -*-

(in-package "KEIM")

(defmethod pdsn~metavars ((node pdsn+node))
  nil)

(defmethod pdsn~metavars ((node pdsn+schematic-node))
  (remove-if-not 'meta~p (data~free-variables (node~formula node))))

#{
(defun pdsn~create (label hyps wff just)
  (declare (edited  "14-APR-1999"  "28-JUL-92 10:00")
           (authors Lassaad NESMITH)
           (input   "A label, a list of hypotheses (other nodes), a formula and a justification.")
           (effect  "None.")
           (value   "A new node created with input values."))
  (let ((metavars (remove-if-not 'meta~p (data~free-variables wff))))
    (if metavars
	(values (make-instance 'pdsn+schematic-node
			       :name label
			       :hyps hyps
			       :formula wff
			       :justification just)
		metavars)
      (values (make-instance 'pdsn+node
			     :name label
			     :hyps hyps
			     :formula wff
			     :justification just)
	      nil)))
    )
#}
