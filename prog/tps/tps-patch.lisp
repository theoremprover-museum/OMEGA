(in-package :omega)

(mod~defmod TPS-PATCH 
            :uses (just keim node pdsn term tps-com)
            :documentation "Some patches and hacks"
            :exports (
                      
                      ))

;;; The following functions are internal in other modules and should not be used:
;;; (orules=formula-hyp-p rule=figure-would-be-hyps rule=map-intern2extern rule=mapping-failed rule=match-modified-postconds rule=warn)


(defun rule=match-modified-postconds (mapping postconds)
	; here check to see if the hypotheses would match up if this rule were called
  (declare
   (authors nesmith)
   (input "A mapping and the list of postconditions")
   (value "The mapping, marked as failed if: 1. a postcondition that would be
justified because of this rule would have, as justifying lines, lines whose
hypotheses are not contained in those of the justified line.  
2. A line would have justify itself, directly or indirectly (circular justifications)."))
  (dolist (postcond postconds)
    (let* ((real-line (cdr (assoc (keim~name postcond)
				  (rule=map-intern2extern mapping)
				  :key #'keim~name
				  :test #'string-equal)))
	   (would-be-hyps (rule=figure-would-be-hyps postcond real-line 
						     mapping))
	   (flag nil))
      (when (dolist (node (set-difference would-be-hyps (pdsn~hyps real-line) :test
					  #'keim~equal) flag)
	      (unless (or (pdsn~hypothesis-node-p node)
			  (equal (infer~find-method 'tps*hyp) (just~method (node~justification node))))
		(setf flag t)))
	(rule=warn ";;; Something in Chris's Hack went wrong, see end of file tps-patch.lisp")
	(setf ( rule=mapping-failed mapping) t))
	
      (when (find real-line
		  (pdsn~justifying-nodes
		   (remove-if #'null
			      (mapcar #'(lambda (jline)
					  (cdr (assoc (keim~name jline)
						      (rule=map-intern2extern mapping)
						      :key #'keim~name
						      :test #'string-equal)))
				      (just~premises (node~justification postcond))))))
	(rule=warn ";;; Node ~A would directly or indirectly justify itself.~%" real-line)
	(setf (rule=mapping-failed mapping) t))
      )))



