;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; THE FABULOUS SOD-SYSTEM      ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *sod-user-dir* "~ameier/sorgetar/")

;; Blackboard stuff
(load (concatenate 'string *sod-user-dir* "blackboard.lisp"))
(load (concatenate 'string *sod-user-dir* "store.lisp"))
;; The MULTI-System itself
(load (concatenate 'string *sod-user-dir* "sod-control.lisp"))
(load (concatenate 'string *sod-user-dir* "execution-messages.lisp"))
(load (concatenate 'string *sod-user-dir* "job.lisp"))
(load (concatenate 'string *sod-user-dir* "demand.lisp"))
(load (concatenate 'string *sod-user-dir* "metar-reasoner.lisp"))
(load (concatenate 'string *sod-user-dir* "reactions.lisp"))              ;;suggestion-reasoner.lisp"))
(load (concatenate 'string *sod-user-dir* "ref-algs.lisp"))
(load (concatenate 'string *sod-user-dir* "state-des.lisp"))
(load (concatenate 'string *sod-user-dir* "strategy-ks.lisp"))
(load (concatenate 'string *sod-user-dir* "interactive.lisp"))
(load (concatenate 'string *sod-user-dir* "sod-main.lisp"))
;; A FEW BLACKBOARD TYPES
(load (concatenate 'string *sod-user-dir* "def-bb-object-types.lisp"))
;; STRATEGIES AS BLACKBOXES
(load (concatenate 'string *sod-user-dir* "def-strategies-as-blackboxes.lisp"))
;; THE CURRENT REFINEMENT ALGORITHMS
(load (concatenate 'string *sod-user-dir* "pplanner.lisp"))
(load (concatenate 'string *sod-user-dir* "instmeta.lisp"))
(load (concatenate 'string *sod-user-dir* "backtrack.lisp"))
(load (concatenate 'string *sod-user-dir* "exp.lisp"))
;; THE CURRENT REFINEMENT ALGORITHM DECLARED
(load (concatenate 'string *sod-user-dir* "def-refinement-algorithms-current.lisp"))
;; MY COMMANDS
(load (concatenate 'string *sod-user-dir* "commands.lisp"))


;; A few strategies + control rules
(load (concatenate 'string *sod-user-dir* "def-strategies-current.lisp"))
(load (concatenate 'string *sod-user-dir* "def-crules-current.lisp"))
