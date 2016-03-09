(in-package "KEIM")

(defun pdsj~justs-equal-at-this-level (just1 just2)
  (declare (edited  "22-MAR-2000")
	   (authors Scholl)
	   (input   "Two justifications.")
	   (effect  "None.")
	   (value   "True, if the two justifications are equal at this level."))
  (when (and just1 just2)
    (and (keim~equal (just~method just1) (just~method just2))
	 (keim~equal (just~premises just1) (just~premises just2))
	 (keim~equal (pdsj~parameters just1) (pdsj~parameters just2))
	 (keim~equal (pdsj~subst just1) (pdsj~subst just2)))))

