;;; -*- Syntax: Common-lisp; package: OMEGA; base: 10; mode: keim -*-
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; analogy.com; This file is part of the OMEGA system
;;
;; major updates: 4.10.1999
;; 
;;
;; Authors: Carsten Ullrich
;; email: cullrich@ags.uni-sb.de 
;;
;; For information about this program, write to:                          
;;   OMEGA Project                                                        
;;   AG Siekmann/FB Informatik                                            
;;   Universitaet des Saarlandes                                          
;;   Bau 36, 4. Stock                                                     
;;   D-66041 Saarbruecken                                                 
;;   Germany    
;;
;; For information about the newest version of Omega, see 
;;   http://www.ags.uni-sb.de/~omega/
;;
;; This program is free software; it can be used under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either version 2 of
;; the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; merchantibility or fitness for a particular purpose. 
;; See the GNU General Public License (http://www.fsf.org/copyleft/gpl.html)
;; for more details.
;;
;;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; the commands for analogy

(in-package "OMEGA")



  
(com~defcommand analogy-step-mode
		(function ana=toggle-step-mode)
		(argnames)
		(argtypes)
		(arghelps)
		(frag-cats analogy)
		(log-p nil)
		(help "Toggles the stepping mode of analogy."))



(defun ana=toggle-step-mode()
  (omega~message "The stepping mode of analogy is now ~:[off~;on~]."
		 (setf ana*step-mode (not ana*step-mode))))


(com~deffragment internal-analogy
                 (uses-comms-of direct-display)
                 (help "Commands for internal analogy."))

(com~deffragment external-analogy
                 (uses-comms-of direct-display)
                 (help "Commands for external analogy."))


(com~defcommand solve-analogous-to-proof
		(function ana=solve-analogous)
		(argnames source)
		(argtypes anything)
		(arghelps "The name of the source-proof")
		(frag-cats analogy external-analogy);planning)
		(log-p T)
		(help "Solves the current problem analogous to the source problem."))

(defun ana=solve-analogous (source)
  (if (prob~find-proof source)
      (ana~exa omega*current-proof-plan source)
    (omega~warn "There is no problem named ~s!" source)))

(com~defcommand solve-analogous-to-node
		(function ana=solve-analogous-to-node)
		(argnames source target)
		(argtypes ndline ndline)
		(arghelps "The name of the source node" "The name of the target node")
		(frag-cats analogy internal-analogy);planning)
		(log-p T)
		(help "Solves the target node analogous to the source node."))


(defun ana=solve-analogous-to-node (source target)
  (ana~ina source target))
;  (plan~step-plan-with (meth~find-method 'internal-analogy-s-b)
;                       target
;                       nil
;                       (list source)
;                       ))

(com~defcommand analogy-interactive-mode
		(function ana=set-interactive-mode)
		(argnames)
		(argtypes)
		(arghelps)
		(frag-cats analogy)
		(log-p T)
		(help "Changes the interactive mode for analogy"))

(defun ana=set-interactive-mode()
  (omega~message "Analogy is now ~:[non-interactive~;interactive~]."
		 (setf ana*interactive-mode (not ana*interactive-mode))))

(com~defcommand cn
		(function ana=close-node)
		(argnames node)
		(argtypes ndline)
		(arghelps "")
		(frag-cats no-gui)
		(log-p T)
		(help "Needed for analogy demos. Internal use only."))

(com~defcommand on
		(function ana=open-node)
		(argnames node)
		(argtypes ndline)
		(arghelps "")
		(frag-cats no-gui)
		(log-p T)
		(help "Needed for analogy demos. Internal use only."))

(defun ana=close-node (node)
  (plan~step-plan-with (meth~find-method 'AnalogyClose-m-b)  node ()()
		       omega*current-proof-plan))

(defun ana=open-node (node)
  (progn
    (pds~open-node! node)
    (setf (pds~agenda omega*current-proof-plan)
	  (agenda~generate (list
			    (mapcar #'agenda~create-goal
				    (pds~open-nodes omega*current-proof-plan)))))
    ))

(com~defcommand set-source-problem
		(function ana=set-source-problem)
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats analogy external-analogy)
		(log-p T)
		(help "Sets the ana*source-plan to the omega*current-proof-plan"))

(defun ana=set-source-problem ()
  (setf ana*source-plan omega*current-proof-plan))

(com~defcommand unset-source-problem
		(function ana=unset-source-problem)
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats analogy external-analogy)
		(log-p T)
		(help "Sets the ana*source-plan to nil"))

(defun ana=unset-source-problem ()
  (setf ana*source-plan nil))





(com~deffragment cplanner
                 (uses-comms-of direct-display)
                 (help "Commands for internal analogy."))

(com~defcommand show-source-plans
		(function ana=print-source-plans)
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats analogy cplanner)
		(log-p T)
		(help "Shows all possible source plans."))

(defun ana=print-source-plans ()
  (omega~message "~A" (ana~source-plans)))

(com~defcommand show-source-steps
		(function ana~print-steps)
		(argnames source-plan)
		(argtypes proof-plan)
		(arghelps "The source plan")
		(frag-cats analogy cplanner)
		(log-p T)
		(defaults ((ana=source-plan-default)))
		(help "Shows the steps of the source plan."))

(defun ana=source-plan-default ()
  (first (ana~source-plans)))

(com~defcommand show-steps
		(function ana=print-current-steps)
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats analogy cplanner)
		(log-p T)
		(help "Shows the steps of the source plan."))

(defun ana=print-current-steps ()
  (ana~print-steps omega*current-proof-plan))

(com~defcommand analogy
  (function ana=analogy)
  (argnames analogy-p)
  (argtypes boolean)
  (arghelps "T to activate analogy, else nil")
  (frag-cats analogy cplanner)
  (defaults ((ana=analogy-default)))
  (help "If argument is T, then analogy is activated."))

(defun ana=analogy-default ()
  (not ana*active))

(defun ana=analogy (bool)
  (setf ana*active bool)
  (ana~activate bool))

(com~defcommand trace-analogy
  (function ana=trace-analogy)
  (argnames trace-analogy-p)
  (argtypes boolean)
  (arghelps "T to trace the analogy process, else nil")
  (frag-cats analogy cplanner)
  (defaults ((ana=trace-analogy-default)))
  (help "If argument is T, then trace the analogy process."))

(defun ana=trace-analogy-default ()
  (not ana*trace))

(defun ana=trace-analogy (bool)
  (setf ana*trace bool))

(com~defcommand trace-cor
  (function ana=trace-cor)
  (argnames trace-cor-p)
  (argtypes boolean)
  (arghelps "T to trace the correspondences, else nil")
  (frag-cats analogy cplanner)
  (defaults ((ana=trace-cor-default)))
  (help "If argument is T, then trace the correspondences."))

(defun ana=trace-cor-default ()
  (not ana*table-verbose))

(defun ana=trace-cor (bool)
  (setf ana*table-verbose bool))

