;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1997 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 2. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: omega@ags.uni-sb.de                                    ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;    LEO COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(com~defcommand call-leo-on-node
  (argnames node tactic supports time-bound theory-list defs-list insert-flag)
  (argtypes ndline string ndline-list integer symbol-list symbol-list boolean)
  (arghelps "Node to prove with LEO"
	    "The tactic to be used by LEO"
	    "The support nodes"
	    "Time bound for proof attempt"
	    "Theories whose definitions will be expanded"
	    "Symbols whose definitions will not be expanded"
	    "A flag indicating whether a partial result will be automatically inserted.")
  (frag-cats extern)
  (function oc=call-leo-on-node)
  (defaults ((oc~default-current-planline) (lc=default-tactic)
	     (leo~direct-supports (oc~default-current-planline))
	     100 (list 'all) (list '= 'defined 'equiv)
	     nil))
  (log-p T)
  (help ""))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  TPS COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-tps-on-node
  (argnames node resource mode allow-rulep search-strategy defs)
  (argtypes ndline integer string boolean string thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
            "A special mode for TPS  (e.g. MODE-THM15B-PR97-A  MODE-CD-LATTICE-THM
MODE-GRP-COMM2-MS98 MODE-DISTRIB-THM-MS98 MODE-THM270-MS98 BASIC-MS04-2-MODE)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "The mating-search strategy
(e.g. MS88,MS89,MS91-6,MS92-9,MS93-1,MS90-3,MS90-9,MS91-7,MS98-1,MS04-2),
in case you want to override the mating-search strategy
associated with the mode.
Leave this NIL to use the mode's mating-search strategy."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern)
  (function tps=call-tps)
  (defaults ((oc~default-current-planline)
             60 'BASIC-MS04-2-MODE t nil nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS on this problem."))


(com~defcommand call-tps-mode-bool-prop-2-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-bool-prop-mode2)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode BOOL-PROP-MODE2 on this problem."))

(com~defcommand call-tps-mode-bool-prop-1-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-bool-prop-mode)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode BOOL-PROP-MODE on this problem."))

(com~defcommand call-tps-mode-gazing-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "The name for the tps-problem."
	    "The directory to write the files in."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-gazing-mode)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode GAZING-MODE on this problem."))

(com~defcommand call-tps-mode-gazing-2-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-gazing-mode2)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode GAZING-MODE2 on this problem."))

(com~defcommand call-tps-mode-ms98-ho-1-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-ms98-ho-mode)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode MS98-HO-MODE on this problem."))

(com~defcommand call-tps-mode-ms98-ho-2-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-ms98-ho-mode2)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode MS98-HO-MODE2 on this problem."))

(com~defcommand call-tps-mode-ms98-fo-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-ms98-fo-mode)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode MS98-FO-MODE on this problem."))

(com~defcommand call-tps-mode-ms98-ho-primsubs-on-node
  (argnames node resource allow-rulep defs)
  (argtypes ndline integer boolean thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A time resource in seconds (integer)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-ms98-ho-primsubs)
  (defaults ((oc~default-current-planline)
	     60 t nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS with mode MS98-HO-PRIMSUBS on this problem."))



(com~defcommand call-tps-interactive-on-node
  (argnames node   tpsname dir-name supersede mode search-strategy defs)
  (argtypes ndline string  creating-directory boolean string string thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "The name for the tps-problem."
	    "The directory to write the files in."
	    "A flag whether existing files in DIR-NAME may be deleted."
            "A special mode for TPS  (e.g. MODE-THM15B-PR97-A  MODE-CD-LATTICE-THM
MODE-GRP-COMM2-MS98 MODE-DISTRIB-THM-MS98 MODE-THM270-MS98)."
	    "The mating-search strategy
(e.g. MS88,MS89,MS91-6,MS92-9,MS93-1,MS90-3,MS90-9,MS91-7,MS98-1),
in case you want to override the mating-search strategy
associated with the mode.
Leave this NIL to use the mode's mating-search strategy."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-interactive)
  (defaults ((oc~default-current-planline)
	     (format nil "tps-problems-~A" (sys~getenv "USER"))
	     "/tmp/" t 'MS98-HO-MODE nil nil))
  (log-p T) 
  (help "Extract an open node with his supports and start TPS on this problem."))


(com~defcommand call-tps-with-supports
  (argnames node supports resource mode allow-rulep search-strategy defs)
  (argtypes ndline ndline-list integer string boolean string thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A list of support lines."
	    "A time resource in seconds (integer)."
            "A special mode for TPS  (e.g. MODE-THM15B-PR97-A  MODE-CD-LATTICE-THM
MODE-GRP-COMM2-MS98 MODE-DISTRIB-THM-MS98 MODE-THM270-MS98)."
	    "A flag whether fancy propositional rules like RuleP are allowed."
	    "The mating-search strategy
(e.g. MS88,MS89,MS91-6,MS92-9,MS93-1,MS90-3,MS90-9,MS91-7,MS98-1),
in case you want to override the mating-search strategy
associated with the mode.
Leave this NIL to use the mode's mating-search strategy."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-with-supports)
  (defaults tps=ctws-defaults)
  (log-p T) 
  (help "Extract an open node with its supports and start TPS on this problem."))

(com~defcommand call-tps-interactive-with-supports
  (argnames node supports tpsname dir-name supersede mode search-strategy defs)
  (argtypes ndline ndline-list string  creating-directory boolean string string thy-ass-list)
  (arghelps "Open node, input for tps with its supports."
	    "A list of support lines."
	    "The name for the tps-problem."
	    "The directory to write the files in."
	    "A time resource in seconds (integer)."
	    "A flag whether existing files in DIR-NAME may be deleted."
            "A special mode for TPS  (e.g. MODE-THM15B-PR97-A  MODE-CD-LATTICE-THM
MODE-GRP-COMM2-MS98 MODE-DISTRIB-THM-MS98 MODE-THM270-MS98)."
	    "The mating-search strategy
(e.g. MS88,MS89,MS91-6,MS92-9,MS93-1,MS90-3,MS90-9,MS91-7,MS98-1),
in case you want to override the mating-search strategy
associated with the mode.
Leave this NIL to use the mode's mating-search strategy."
	    "Definitions that are not to be transmitted.")
  (frag-cats extern tps)
  (function tps=call-tps-interactive-with-supports)
  (defaults tps=ctiws-defaults)
  (log-p T) 
  (help "Extract an open node with its supports and start TPS on this problem."))



(com~defcommand show-tps-proof
  (argnames node)
  (argtypes ndline)
  (arghelps "A TPS justified node node.")
  (frag-cats extern tps)
  (function tps=show-tps-proof)
  (defaults ((tps=last-tps-justified-line)))
  (log-p T) 
  (help "Show the lines of the TPS-print-file."))




;;;;;;;;;;;;;;;;;;;;;;;;;
;; Satchmo
;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand call-satchmo-on-node
  (argnames node dir ressource)
  (argtypes ndline creating-directory integer)
  (arghelps "Node to prove with SATCHMO"
	    "The (writable!) directory for depositing SATCHMO auxiliary files"
	    "A time ressource in seconds (integer)."
	    )
  (frag-cats extern)
  (function oc=command-call-satchmo-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-satchmo-node)
	     (atptop~default-directory)
	     10))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;
;; OTTER
;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-otter-on-node
  (argnames node dir mode expand proof-object user-flag-string user-weight-string ressource
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline creating-directory symbol symbol boolean string string integer
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with OTTER"
	    "The (writable!) directory for depositing OTTER auxiliary files"
	    "Mode for calling OTTER (auto/user/combined)"
	    "A proof found by OTTER is used to (test/parse/expand)"
	    "Use build_proof_object"
	    "A string of user flag-settings or a file-name"
	    "A string of user weight-settings or a file-name"
	    "A time ressource in seconds (integer)."
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=command-call-otter-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)
	     (atptop~default-directory)
	     'auto 'expand 't (string "") (string "") 10 
	     'auto nil nil 2 't 't 'constants
	     ))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;
;; BLIKSEM
;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-bliksem-on-node
  (argnames node dir ressource expand command-string
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline creating-directory integer symbol string
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with BLIKSEM"
	    "The (writable!) directory for depositing BLIKSEM auxiliary files"
	    "A time ressource in seconds (integer)."
	    "A proof found by BLIKSEM is used to (test/parse/expand)"
	    "The commando settings (string)"
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=command-call-bliksem-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-bliksem-node)
	     (atptop~default-directory)
	     10
	     'expand
	     (string "")
	     'auto nil nil 2 't 't 'constants))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;
;; EQP
;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-eqp-on-node
  (argnames node dir ressource expand command-string
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline creating-directory integer symbol string
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with EQP"
	    "The (writable!) directory for depositing EQP auxiliary files"
	    "A time ressource in seconds (integer)."
	    "A proof found by EQP is used to (test/parse/expand)"
	    "The commando settings (string)"
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=command-call-eqp-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-eqp-node)
	     (atptop~default-directory)
	     10
	     'expand
	     (string "")
	     'auto nil nil 2 't 't 'constants))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;
;; WALDMEISTER
;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-waldmeister-on-node
  (argnames node dir ressource expand command-string
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline creating-directory integer symbol string
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with WALDMEISTER"
	    "The (writable!) directory for depositing WALDMEISTER auxiliary files"
	    "A time ressource in seconds (integer)."
	    "A proof found by WALDMEISTER is used to (test/parse/expand)"
	    "The commando settings (string)"
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=command-call-wald-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-wald-node)
	     (atptop~default-directory)
	     10
	     'expand
	     (string "")
	     'auto nil nil 2 't 't 'constants))
  (log-p T) 
  (help ""))



;;;;;;;;;;;;;;;;;;;;;;;;
;; Spass
;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand call-spass-on-node
  (argnames node dir expand auto-mode splitting-level ressource
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline creating-directory symbol boolean integer integer
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with SPASS"
	    "The (writable!) directory for depositing SPASS auxiliary files"
	    "A proof found by SPASS is used to (test/parse/expand)"
	    "Using auto mode for calling Spass (t/nil)"
	    "Splitting Levels"
	    "A time ressource in seconds (integer)."
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=command-call-spass-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-spass-node)
	     (atptop~default-directory)
	     'expand
	     't
	     0
	     10
	     'auto nil nil 2 't 't 'constants))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROTEIN
;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-protein-on-node
  (argnames node dir expand ressource
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline creating-directory symbol integer
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with PROTEIN"
	    "The (writable!) directory for depositing PROTEIN auxiliary files"
	    "A proof found by PROTEIN is used to (test/parse/expand)"
	    "A time ressource in seconds (integer)."
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=command-call-protein-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-protein-node)
	     (atptop~default-directory)
	     'expand
	     10
	     'auto nil nil 2 't 't 'constants))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PL-ATP:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-pl-atp-on-node
  (argnames node dir mode expand proof-object user-flag-string user-weight-string ressource
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline creating-directory symbol symbol boolean string string integer
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with PL-ATP"
	    "The (writable!) directory for depositing PL-ATP auxiliary files"
	    "Mode for calling PL-ATP/OTTER (auto/user/combined)"
	    "A proof found by PL-ATP/OTTER is used to (test/parse/expand)"
	    "Use build_proof_object"
	    "A string of user flag-settings or a file-name"
	    "A string of user weight-settings or a file-name"
	    "A time ressource in seconds (integer)."
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=command-call-pl-atp-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-pl-atp-node)
	     (atptop~default-directory)
	     'auto 'expand 't (string "") (string "") 10 
	     'auto nil nil 2 't 't 'constants))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allgemein F.O. ATP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand call-fo-atp-on-node
  (argnames node atp dir expand ressource
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline symbol creating-directory symbol integer
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with FO ATP"
	    "ATP to call (Otter, Spass, Bliksem, Waldmeister, Protein, or Eqp)"
	    "The (writable!) directory for depositing the ATP auxiliary files"
	    "A proof found by ATP is used to (test/parse/expand)"
	    "A time ressource in seconds (integer)."
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full)."
	    )
  (frag-cats extern)
  (function oc=command-call-foatp-on-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-foatp-node)
	     'otter
	     (atptop~default-directory)
	     'expand
	     10
	     'auto nil nil 2 't 't 'constants
	     ))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trans RES PROOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand transform-resolution-proof
  (argnames res-proof prover expand sspu-style indirect-proof integral-formulas maximal-depth tnd avoid-doubling lemmas)
  (argtypes resolution-proof symbol symbol symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Resolution proof to transform"
	    "Is the resolution proof found by otter, spass, protein or pl-atp"
	    "Should the resolution proof be expanded for itself (alone) or into an existing PDS (name of existing PDS)"
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/constants/full).")
  (frag-cats extern)
  (function oc=command-transform)
  (defaults (omega*current-resolution-proof 'otter 'alone 'auto nil nil 2 't 't 'constants))
  (log-p T)
  (help "Transform the resolution proof into an ndproof."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; call concurrent atp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(com~defcommand call-concurrent-atp
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to prove with concurrent atps")
  (frag-cats extern)
  (function oc=call-concurrent-atp)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)
	    ))
  (log-p T) 
  (help "Given a node to proof by atps, all possible atp-problems are produced and the according"
	"atps are called by LOUI."))

(com~defcommand call-concurrent-fo-atp
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to prove with concurrent fo atps")
  (frag-cats extern)
  (function oc=call-concurrent-atp-fo)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)
	    ))
  (log-p T) 
  (help "Given a node to proof by atps, all possible atp-problems are produced and the according"
	"atps are called by LOUI."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; choose one resolution proof to transform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand choose-atp-problem-to-transform
  (argnames node)
  (argtypes ndline)
  (arghelps "Node to choose a problem from")
  (frag-cats extern)
  (function oc=choose-problem-to-transform)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node)	     
	     ))
  (log-p T) 
  (help ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; READ THE Output of an ATP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand read-and-transform-atp-output-for-node
  (argnames node file
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline existing-file
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node for which an ATP proof exists"
	    "The ATP out file"
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=read-and-transform-atp-output-node)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node) nil
	     'auto nil nil 2 't 't 'constants))
  (log-p T) 
  (help "Given an open node and an ATP proof for this node in a file. The file is read and the proof is translated into a proof for"
	"the node."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSFORM TSTP PROOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand transform-tstp-proof
  (argnames node proof-object		
	    sspu-style indirect-proof integral-formulas maximal-depth thn avoid-doubeling lemmas)
  (argtypes ndline boolean
	    symbol boolean boolean integer boolean boolean symbol)
  (arghelps "Node to prove with TSTP proof object"
	    "Is the style of the otter out file with build_proof_object"
	    "The SSPU-style (direct/compact/auto)"
	    "Indirect proof"
	    "Integral formulas"
	    "Maximal depth of searching integral-formulas"
	    "Prefer tertium non datur case analyses"
	    "Avoid doubling"
	    "Lemmas over (nil/free/constants/full).")
  (frag-cats extern)
  (function oc=transform-tstp-proof)
  (defaults ((oc=default-current-plan-or-untested-or-unexpanded-otter-node) nil
	     'auto nil nil 2 't 't 'constants))
  (log-p T))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TSTP-OUTPUT-2-PROBLEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand tstp2problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern)
  (function tstp=initialclauses2problem)
  (defaults )
  (log-p T))




