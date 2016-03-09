
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; OANTS Commands Module
;;                            (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the commands necessary to work with the
;; OMEGA-Ants system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :omega)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands for automating OANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand automate
  (function auto~prove)
  (frag-cats omega no-gui)
  (log-p T)
  (help "Automate the application of distributedly computated command suggestions."))

(com~defcommand stop-automate
  (function auto~stop)
  (frag-cats omega no-gui)
  (log-p T)
  (help "Stop automatically applying command suggestions."))

(com~defcommand suspend-automate
  (function auto~suspend)
  (frag-cats omega no-gui)
  (log-p T)
  (help "Suspend the automatic application of command suggestions."))

(com~defcommand resume-automate
  (function auto~resume)
  (frag-cats omega no-gui)
  (log-p T)
  (help "Resume the automatic application of command suggestions."))

(com~defcommand trace-automate
  (function auto~trace)
  (frag-cats omega no-gui)
  (log-p T)
  (help "Trace the automate process."))

(com~defcommand untrace-automate
  (function auto~untrace)
  (frag-cats omega no-gui)
  (log-p T)
  (help "Untrace the automate process."))

(com~defcommand auto-default-interval
  (argnames seconds)
  (argtypes integer)
  (arghelps "Set the default execution interval for the O-ANTS theorem prover.")
  (frag-cats extern)
  (function auto~set-auto-default-interval)
  (defaults (auto*default-interval))
  (log-p T) 
  (help ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands that deal with external reasoners
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand show-names-of-advertised-proofs
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to prove with LEO")
  (frag-cats extern)
  (function agplan~advertised-proofs)
  (defaults ((oc~default-current-planline)))
  (log-p T)
  (help ""))


(com~defcommand show-advertised-proof
  (argnames name)
  (argtypes string)
  (arghelps "A string referring to the automatically found proof or a node that
has been automatically proven.")
  (frag-cats extern)
  (function agplan~show-pds)
  (defaults ((agplan~best-advertised-proof)))
  (log-p T) 
  (help ""))


(com~defcommand show-original-proof
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function agplan~show-orig-pds)
  (defaults )
  (log-p T) 
  (help ""))

       
(com~defcommand apply-advertised-proof
  (argnames name)
  (argtypes string)
  (arghelps "A string referring to the automatically found proof or a node that
has been automatically proven.")
  (frag-cats extern)
  (function agplan~apply-advertised-pds)
  (defaults ((agplan~best-advertised-proof)))
  (Log-p T) 
  (help ""))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Categories for Nic Tactics and External Reasoners etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;now in omega-com.lisp MP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Commands that deal with NIC Agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand use-nic-agents
  (function nic~use-nic-agents)
  (frag-cats omega no-gui)
  (help "Use only agents of the NIC calculus."))

(com~defcommand use-nic-heuristics
  (function nic~use-nic-heuristics)
  (frag-cats omega no-gui)
  (help "Use the heuristics for the NIC calculus."))

(com~defcommand use-external-agents
  (function nic~use-external-agents)
  (frag-cats omega no-gui)
  (help "Use only agents for external reasoners."))

(com~defcommand add-external-agents
  (function nic~add-external-agents)
  (frag-cats omega no-gui)
  (help "Add agents for external reasoners to the already considered agents."))

(com~defcommand add-command-agents
  (argnames commands)
  (argtypes command-list)
  (arghelps "A list of commands")
  (function nic~add-command-agents)
  (defaults ((oc=set-command-agents-default)))
  (frag-cats omega no-gui)
  (help "Adds a list of agents to the commands agents used for command suggestions."))

