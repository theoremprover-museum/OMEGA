(in-package :omega)


(com~deffragment constraint-solver-CoSIE
		 (uses-comms-of direct-display)
		 (help "Commands for using and handling CoSIE"))

(com~defcommand Restart
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern constraint-solver-cosie)
		(function CoSIE~announce)
		(log-p T)
		(help "Re-connecting to the CoSIE constraint solver
creating a new service object."))


(com~defcommand Search
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern constraint-solver-cosie)
		(function cscom=solve-cosie)
		(log-p T)
		(help "Tell CoSIE to search for instantiations for the
problem variables."))

(defun cscom=solve-cosie ()
  (let ((subst (cosie~call 'solve)))
    (omega~message "~& search: ~S" subst)
    (if (subst~p subst)
	(progn (setf plan*already-done nil)
	       (omega~message "~%Instantiating meta-variable(s) ~S" (subst~domain subst))
	       (plan=instantiate-pds omega*current-proof-plan
				     subst))
      (progn (omega~message "~& CoSIE couldn't find instantiations for
the meta-variables.")
	     nil))))

(com~defcommand Show-Constraints
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern constraint-solver-cosie)
		(function cscom=show-constraints)
		(log-p T)
		(help "Opens a Window and prints the constraint stores."))

(defun cscom=show-constraints ()
  (cosie~call 'show))


(com~defcommand Show-Context-Tree
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern constraint-solver-cosie)
		(function cscom=show-context-tree)
		(log-p T)
		(help "Opens a Window and shows the context tree."))

(defun cscom=show-context-tree ()
  (cosie~call 'daVinci))


#|
(com~defcommand show-CoSIE
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats planning)
		(function CoSIE~show)
		(log-p T)
		(help ""))

(com~defcommand Solve-CoSIE
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern constraint-solver)
		(function CoSIE~solve)
		(log-p T)
		(help ""))

(com~defcommand Show-CoSIE
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern constraint-solver)
		(function CoSIE~show)
		(log-p T)
		(help ""))


(com~defcommand Trace-CoSIE
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats extern constraint-solver)
		(function CoSIE~trace)
		(log-p T)
		(help ""))

|#
