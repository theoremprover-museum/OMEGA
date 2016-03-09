
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Automate Commands Module
;;                            (sorge@ags.uni-sb.de)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains the commands necessary to work with the
;; OMEGA-Ants system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :omega)

(com~defcommand automate
  (function auto~prove)
  (frag-cats omega)
  (log-p T)
  (help "Automate the application of distributedly computated command suggestions."))

(com~defcommand stop-automate
  (function auto~stop)
  (frag-cats omega)
  (log-p T)
  (help "Stop automatically applying command suggestions."))

(com~defcommand suspend-automate
  (function auto~suspend)
  (frag-cats omega)
  (log-p T)
  (help "Suspend the automatic application of command suggestions."))

(com~defcommand resume-automate
  (function auto~resume)
  (frag-cats omega)
  (log-p T)
  (help "Resume the automatic application of command suggestions."))

(com~defcommand trace-automate
  (function auto~trace)
  (frag-cats omega)
  (log-p T)
  (help "Trace the automate process."))

(com~defcommand untrace-automate
  (function auto~untrace)
  (frag-cats omega)
  (log-p T)
  (help "Untrace the automate process."))

