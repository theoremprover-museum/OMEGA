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
;;;;;;;;;    LEO INTERACTIVE COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(com~defcommand leo-initialize
  (argnames node tactic theory-list)
  (argtypes ndline string symbol-list)
  (arghelps "Node to prove with LEO"
	    "The tactic to be used by LEO"
	    "Theories whose definitions will be expanded")
  (frag-cats extern leo-interactive)
  (function oc=leo-initialize)
  (defaults ((oc~default-current-planline) (lc=default-tactic) ()))
  (log-p T)
  (help ""))


(com~defcommand show-clauses
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern leo-interactive)
  (function lc=show-clauses)
  (log-p T)
  (help "Displays the two clause sets: the set of support (LEO*G-SOS) and the set of usable clauses (LEO*G-USABLE)."))


(com~defcommand show-clause
  (argnames Name)
  (argtypes string)
  (arghelps "of the clause to show")
  (frag-cats extern leo-interactive)
  (function lc=show-clause)
  (log-p T)
  (help "Displays a clause, determined by name."))


(com~defcommand leo-help
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern leo-interactive)
  (function lc=help)
  (log-p T)
  (help "Lists all commands together with a short description."))


(com~defcommand resolve
  (argnames Name1 Name2 Position1 Position2)
  (argtypes string string integer integer)
  (arghelps "of a clause" "of a clause" "in clause 1" "in clause 2")
  (frag-cats extern leo-interactive)
  (function lc=resolve)
  (log-p T)
  (help "Applies resolution rule on two clauses."))


(com~defcommand para
  (argnames Name1 Name2 Position1 Position2 Direction)
  (argtypes string string integer integer symbol)
  (arghelps "of a clause" "of a clause" "literal in clause 1" "equation in clause 2" "direction of equation")
  (frag-cats extern leo-interactive)
  (function lc=para)
  (log-p T)
  (help "Applies paramodulation rule on two clauses."))


(com~defcommand prim-subst
  (argnames Name Position)
  (argtypes string integer)
  (arghelps "of a clause" "in clause")
  (frag-cats extern leo-interactive)
  (function lc=prim-subst)
  (log-p T)
  (help "Applies primitive substitution rule on a clause."))


(com~defcommand pre-unify
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause")
  (frag-cats extern leo-interactive)
  (function lc=pre-unify)
  (log-p T)
  (help "Applies pre-unification on a clause."))


(com~defcommand clash-pre-unify
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause")
  (frag-cats extern leo-interactive)
  (function lc=clash-pre-unify)
  (log-p T)
  (help "Applies pre-unification on a clause."))


(com~defcommand pre-unifiers
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause")
  (frag-cats extern leo-interactive)
  (function lc=pre-unifiers)
  (log-p T)
  (help "Computes the pre-unifiers of a clause."))



(com~defcommand ext
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause" "of a uni-constraint")
  (frag-cats extern leo-interactive)
  (function lc=extensionality)
  (log-p T)
  (help "Applies extensionality rule on a clause."))


(com~defcommand ext-input
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause" "of a uni-constraint")
  (frag-cats extern leo-interactive)
  (function lc=extensionality-input)
  (log-p T)
  (help "Applies extensionality rule on a clause."))


(com~defcommand ext-short
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause" "of a uni-constraint")
  (frag-cats extern leo-interactive)
  (function lc=extensionality-short)
  (log-p T)
  (help "Applies extensionality rule on a clause."))


(com~defcommand ext-recursive
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause")
  (frag-cats extern leo-interactive)
  (function lc=extensionality-recursive)
  (log-p T)
  (help "Applies extensionality rule on a clause."))


(com~defcommand ext-new
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause" "of a uni-constraint")
  (frag-cats extern leo-interactive)
  (function lc=extensionality-new)
  (log-p T)
  (help "Applies extensionality rule on a clause."))


(com~defcommand ext-parallel
		(argnames Name)
		(argtypes string)
		(arghelps "of a clause")
		(frag-cats extern leo-interactive)
		(function lc=extensionality-parallel)
		(log-p T)
		(help "Applies extensionality rule parallel on a clause."))


(com~defcommand decompose
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause")
  (frag-cats extern leo-interactive)
  (function lc=decompose)
  (log-p T)
  (help "Applies decomposition on a clause."))


(com~defcommand imitate
  (argnames Name Position1)
  (argtypes string integer)
  (arghelps "of clause" "in clause")
  (frag-cats extern leo-interactive)
  (function lc=imitate)
  (log-p T)
  (help "Applies imitation rule on a clause."))


(com~defcommand project
  (argnames Name Position1)
  (argtypes string integer)
  (arghelps "of clause" "in clause")
  (frag-cats extern leo-interactive)
  (function lc=project)
  (log-p T)
  (help "Applies projection rule on a clause."))


(com~defcommand proj-imi
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause")
  (frag-cats extern leo-interactive)
  (function lc=proj-imi)
  (log-p T)
  (help "Applies (extensionallly) proj-imi rule on a clause."))


(com~defcommand factorize
  (argnames Name Position1 Position2)
  (argtypes string integer integer)
  (arghelps "of clause" "in clause" "in clause")
  (frag-cats extern leo-interactive)
  (function lc=factorize)
  (log-p T)
  (help "Applies factorization rule on a clause."))


(com~defcommand subsumes
  (argnames Name1 Name2)
  (argtypes string string)
  (arghelps "of clause 1" "of clause 2")
  (frag-cats extern leo-interactive)
  (function lc=subsumes)
  (log-p T)
  (help "Determines whether a clause subsumes another clause."))


(com~defcommand subsumes-new
  (argnames Name1 Name2)
  (argtypes string string)
  (arghelps "of clause 1" "of clause 2")
  (frag-cats extern leo-interactive)
  (function lc=subsumes-new)
  (log-p T)
  (help "Determines whether a clause subsumes another clause."))


(com~defcommand set-flag
  (argnames Flag Value)
  (argtypes symbol anything)
  (arghelps "Name of flag" "New Value")
  (frag-cats extern leo-interactive)
  (function lc=set-flag)
  (log-p T)
  (help "Sets a global flag"))


(com~defcommand verbose
  (argnames int)
  (argtypes integer)
  (arghelps "no(0) or half(1) or full(2)")
  (frag-cats extern leo-interactive)
  (function lc=verbose)
  (log-p T)
  (help "Sets the flags leo*f-verbose and leo*f-verbose-half"))


(com~defcommand show-flags
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats extern leo-interactive)
  (function lc=show-flags)
  (log-p T)
  (help "Shows the global flags"))


(com~defcommand show-vars
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats extern leo-interactive)
  (function lc=show-vars)
  (log-p T)
  (help "Shows the global vars."))


(com~defcommand show-tactics
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats extern leo-interactive)
  (function lc=show-tactics)
  (log-p T)
  (help "Shows the tactics."))


(com~defcommand set-tactic
  (argnames Value)
  (argtypes string)
  (arghelps "")
  (frag-cats extern leo-interactive)
  (function lc=set-tactic)
  (log-p T)
  (help "Sets the tactic."))


(com~defcommand leo-prove
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern leo-interactive)
  (function lc=prove)
  (log-p T)
  (help "Prove with default parameter-settings."))


(com~defcommand show-leo-proof
  (argnames node)
  (argtypes ndline)
  (arghelps "A node proven by LEO")
  (frag-cats extern leo-interactive)
  (function lc=show-proof)
  (log-p T)
  (help "Displays the linearized LEO proof"))


(com~defcommand show-derivation
  (argnames Name)
  (argtypes string)
  (arghelps "of a clause")
  (frag-cats extern leo-interactive)
  (function lc=show-derivation)
  (log-p T)
  (help "Displays a linearized derivation of a the clause."))


(com~defcommand write-proof
  (argnames File)
  (argtypes pathname)
  (arghelps "Name of the file to write")
  (frag-cats extern leo-interactive)
  (function lc=write-proof)
  (log-p T)
  (help "Writes the proof in a file."))


(com~defcommand write-louiproof
  (argnames File)
  (argtypes pathname)
  (arghelps "Name of the file to write")
  (frag-cats extern leo-interactive)
  (function lc=write-louiproof)
  (log-p T)
  (help "Writes the proof in LOUI format in a file."))


(com~defcommand write-derivation
  (argnames Name File)
  (argtypes string pathname)
  (arghelps "of a clause" "Name of the file to write")
  (frag-cats extern leo-interactive)
  (function lc=write-derivation)
  (log-p T)
  (help "Writes the derivation of a clause in a file."))


(com~defcommand write-louiderivation
  (argnames Name File)
  (argtypes string pathname)
  (arghelps "of a clause" "Name of the file to write")
  (frag-cats extern leo-interactive)
  (function lc=write-louiderivation)
  (log-p T)
  (help "Writes the derivation of a clause in LOUI format in a file."))


(com~defcommand delete-clause
  (argnames Name)
  (argtypes string)
  (arghelps "of clause to remove")
  (frag-cats extern leo-interactive)
  (function lc=remove)
  (log-p T)
  (help "Deletes a clause from the current environment."))


(com~defcommand save-clause
  (argnames Name )
  (argtypes symbol)
  (arghelps "of the clause to save")
  (frag-cats extern leo-interactive)
  (function lc=save-clause)
  (log-p T)
  (help "Save a clause for use after termination of LEO under its clausename."))


(com~defcommand save-clause-nn
  (argnames Variable Name )
  (argtypes symbol string)
  (arghelps "name to save the clause" "of the clause to save")
  (frag-cats extern leo-interactive)
  (function lc=save-clause-nn)
  (log-p T)
  (help "Save a clause for use after termination of LEO under a new name."))


(com~defcommand leo-beta-normalize
  (argnames Name)
  (argtypes string)
  (arghelps "which clause")
  (frag-cats extern leo-interactive)
  (function lc=beta-normalize)
  (log-p T)
  (help "Normalize a clause."))


(com~defcommand reload
  (argnames Name )
  (argtypes symbol)
  (arghelps "of the system to reload")
  (frag-cats extern leo-interactive)
  (function lc=reload)
  (log-p T)
  (help "Save a clause for use after termination of LEO under its clausename."))


(com~defcommand start-report
		(argnames Name1 Name2)
		(argtypes pathname pathname)
		(arghelps "of the tex report file" "of the html report
file")
		(frag-cats extern leo-interactive)
		(function lc=start-report)
		(log-p T)
		(help "Opens the tex and html report streams."))


(com~defcommand report
		(argnames)
		(argtypes)
		(arghelps)
		(frag-cats extern leo-interactive)
		(function lc=report)
		(log-p T)
		(help "Sends a report entry to stream."))
		

(com~defcommand end-report
		(argnames)
		(argtypes)
		(arghelps)
		(frag-cats extern leo-interactive)
		(function lc=end-report)
		(log-p T)
		(help "Closes the report stream."))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;   LEO NO-GUI COMMANDS (Don't appear in graphical user interface when
;;;;;;;;                        using LEO within OMEGA)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand leo      
  (function oc=leo)
  (argnames)
  (argtypes)
  (arghelps)
  (frag-cats omega change-comint no-gui)
  (defaults)
  (help "Enters the command interpreter for LEO."))


(com~defcommand identify
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern leo-interactive no-gui)
  (function lc=identify)
  (log-p T)
  (help ""))


(com~defcommand read-leo-problem
  (argnames Name )
  (argtypes existing-file)
  (arghelps "of file to read")
  (frag-cats extern leo-interactive no-gui)
  (function lc=load-f)
  (log-p T)
  (help "Read a file, which contains a POST representation of a problem, and transforms this problem into clause normal form."))


(com~defcommand show-leo-problem
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern leo-interactive no-gui)
  (function lc=show-problem)
  (log-p T)
  (help "Displays the given problem (POST input)."))


(com~defcommand exit
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern leo-interactive no-gui)
  (function leo=leave)
  (log-p T)
  (help "Leave the current command interpreter top level."))


(com~defcommand new-log
  (argnames logfile )
  (argtypes pathname)
  (arghelps "Name of a file to log in")
  (frag-cats extern leo-interactive no-gui)
  (function oc=new-log)
  (defaults ((oc~default-logfile)))
  (help "Sets the log mode and the log file name to the given path name."))


(com~defcommand step-log
  (argnames logfile )
  (argtypes existing-file)
  (arghelps "Name of a file to read commands from")
  (frag-cats extern leo-interactive no-gui)
  (function oc=step-log)
  (help "Reads a log file stepwise and eventuelly stores some of its commands in a new log
file."))


(com~defcommand execute-log
  (argnames logfile )
  (argtypes existing-file)
  (arghelps "Name of a file to read commands from")
  (frag-cats extern leo-interactive no-gui)
  (function oc=execute-log)
  (help "Reads a log file stepwise and eventuelly stores some of its commands in a new log
file."))


(com~defcommand cd
		(argnames Name)
		(argtypes pathname)
		(arghelps "of the directory")
		(frag-cats extern leo-interactive no-gui)
		(function lc=cd)
		(log-p T)
		(help "Changes the directory."))


(com~defcommand gui-proof
  (argnames node)
  (argtypes ndline)
  (arghelps "Line proven by LEO")
  (frag-cats extern leo-interactive)
  (function oc=gui-proof)
  (defaults ((oc~default-leo-node)))
  (help "Displays the LEO proof of node in the GUI."))

(com~defcommand gui-orig-context
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats extern leo-interactive)
  (function oc=gui-orig-context)
  (help "Displays the original proof context again in the GUI."))


(com~defcommand expand-def
  (argnames Name Definiendum)
  (argtypes string string)
  (arghelps "of a clause" "of a Definition")
  (frag-cats extern leo-interactive)
  (function lc=expand-def)
  (log-p T)
  (help "Expand the definition in clause and normalizes."))

(com~defcommand expand-defs
  (argnames Name Definiendums)
  (argtypes symbol symbol-list)
  (arghelps "of a clause" "list of a Definitions")
  (frag-cats extern leo-interactive)
  (function lc=expand-defs)
  (log-p T)
  (help "Expand the definitions in clause and normalizes."))
