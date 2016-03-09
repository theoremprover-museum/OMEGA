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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; post2tex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;outputs a TeX (not LaTeX) form of the current nd proof into file
(com~defcommand tex-proof-old
  (argnames outfile tex-style)
  (argtypes pathname tex-style)
  (arghelps "Name of a file to write to" "TeX style to use")
  (frag-cats presentation file-io)
  (function p2tcom=tex-proof)
  (defaults ((p2tcom=tex-proof-file-default)
	     :tex-style-fo-infix))
  (help "Write the current ND proof in TeX form to the file given."))


;;;outputs a LaTeX form of the current ND proof into a file
(com~defcommand tex-proof
  (argnames outfile)
  (argtypes pathname)
  (arghelps "Name of a file to write to")
  (frag-cats presentation file-io)
  (function p2tcom=post2tex-proof)
  (defaults ((p2tcom=tex-proof-file-default)))
  (help "Write the current ND proof in LaTeX form to the file given."))

;;;outputs a LaTeX form of the current plan state into a file

;obsolete MP
;(com~defcommand tex-plan-state
;  (argnames outfile)
;  (argtypes pathname)
;  (arghelps "Name of a file to write to")
;  (frag-cats presentation file-io)
;  (function p2tcom=post2tex-plan-state)
;  (defaults ((p2tcom=tex-proof-file-default)))
;  (help "Write the current plan state in LaTeX form to the file given."))

;;; Write a proof representation of the current proof in LaTeX format to a file
(com~defcommand tex-proof-tree
  (argnames outfile)
  (argtypes pathname)
  (arghelps "Name of a file to write to")
  (frag-cats presentation file-io)
  (function p2tcom=post2tex-proof-tree)
  (defaults ((p2tcom=tex-proof-file-default)))
  (help "Write the current ND proof in LaTeX form to the file given."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROVERB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+outdated(com~defcommand proverb-proof
		(argnames unit-nodes db implicit abstract prefer-formula dir)
		(argtypes ndline-list pathname boolean boolean boolean pathname)
		(arghelps "a list of proof nodes interpreted as subproof roots"
			  "a path to a db.lisp (in PROVERB's examples)"
			  "prefer implicit verbalization"
			  "prefer abstract verbalization"
			  "a boolean indicating whether formulae are preferred to text"
			  "Name of a directory to write LaTeX source to") 
		(frag-cats presentation proverb)
		(log-p t)
		(function oc=proverb-proof)
		(defaults (nil (oc~default-current-proverb-dir) t t t (oc=typeset-dir-default)))
		(help "Macroplanning, creates a list of preverbal messages, the text plan."))

#+outdated(defun oc=proverb-proof (unit-nodes db implicit abstract prefer-formula dir)
  (oc=present!)
  (oc=pm-present unit-nodes db implicit abstract)
  (oc=verbalize prefer-formula)
  (oc=typeset dir))

#+outdated(com~defcommand reset-tag
		(argnames)
		(argtypes)
		(arghelps)
		(frag-cats presentation proverb)
		(function tag::reset-tag)
		(help "Resets TAG-GEN"))

#+outdated(com~defcommand verbalize
		(argnames prefer-formula)
		(argtypes boolean)
		(arghelps "A boolean indicating whether formulae are preferred to text")
		(frag-cats presentation proverb)
		(defaults (t))
		(log-p t)
		(function oc=verbalize)
		(help "Verbalizes the proof"))

#+outdated(com~defcommand typeset
		(argnames dir)
		(argtypes pathname)
		(arghelps "Name of a directory to write LaTeX source to")
		(frag-cats presentation proverb)
		(defaults ((oc=typeset-dir-default)))
		(function oc=typeset)
		(help "Typesetting the proof"))

#+outdated(com~defcommand macroplan
		(argnames unit-nodes db implicit abstract)
		(argtypes ndline-list pathname boolean boolean)
		(arghelps "a list of proof nodes interpreted as subproof roots"
			  "a path to a db.lisp (in PROVERB's examples)"
			  "prefer implicit verbalization"
			  "prefer abstract verbalization") 
		(frag-cats presentation proverb)
		(log-p t)
		(function oc=pm-present)
		;;; altes 2. default argument: (oc~default-current-tp-dir)
		(defaults (nil (oc~default-current-proverb-dir) t t))
		(help "Macroplanning, creates a list of preverbal messages, the text plan."))


(com~defcommand present
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats presentation proverb)
  (log-p t)
  (function oc=present!)
  (help "Presents the current proof which must be closed by deleting not necessary lines."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; P.rex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~deffragment p.rex
		 (help "P.rex commands"))

(com~defcommand Start-Prex
		(argnames)
		(arghelps)
		(frag-cats presentation p.rex)
		(log-p t)
		(function prex~start)
		(defaults)
		(help "Start P.rex")
		)

(com~defcommand Leave-Prex
		(argnames)
		(arghelps)
		(frag-cats presentation p.rex)
		(log-p t)
		(function prex~leave)
		(defaults)
		(help "Leave P.rex")
		)

(com~defcommand Show-Presentation
		(argnames)
		(arghelps)
		(frag-cats presentation p.rex)
		(log-p t)
		(function prex~show)
		(defaults)
		(help "Envoke P.rex")
		)

(com~defcommand Interrupt-Prex
		(argnames)
		(arghelps)
		(frag-cats presentation p.rex)
		(log-p t)
		(function prex~send-interrupt)
		(help "Interrupt the presentation to enter a dialog.")
		)

(com~defcommand set-Prex-layout
		(argnames form)
		(arghelps "one of LATEX or LML")
		(argtypes symbol)
		(frag-cats presentation p.rex)
		(function prex~set-layout))

(com~defcommand Dialog
		(argnames args)
		(argtypes string-list)
		(frag-cats presentation p.rex)
		(log-p t)
		(function prex~send-dialog)
		(help "Pass dialog interaction to P.rex."))

(com~defcommand show-step
		(argnames node)
		(argtypes ndline)
		(frag-cats presentation p.rex)
		(log-p t)
		(function prex~show-step)
		(help "Show one proof step.")
		)

(com~defcommand transform-proof
		(argnames)
		(argtypes)
		(frag-cats presentation p.rex)
		(function prex~transform)
		(help "Transforms the current PDS to its Twega representation."))

(com~defcommand save-twega-proof
		(argnames outfile)
		(argtypes pathname)
		(arghelps "Pathname to a file to write to")
		(frag-cats presentation P.rex)
		(function twpp~SgnWriteToFile)
		(defaults ((prex~default-filename)))
		(help "Saves the Twega represenation of the current proof, which is readable by P.rex."))

(com~defcommand save-rules
		(argnames outfile)
		(argtypes pathname)
		(arghelps "Pathname to file to write to")
		(frag-cats presentation P.rex)
		(function oc=save-rules)
		(defaults ((prex~default-filename)))
		(help "Saves the names of all inference rules available in the current theory."))

(defun oc=save-rules (path)
  (p2t~save-inference-rules (prob~proof-theory omega*current-proof-plan) path))

;; assertion -> generic-tactics

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand compute-refutation-graph
		(argnames res-proof)
		(argtypes resolution-proof)
		(arghelps "Resolution proof to transform")
		(frag-cats presentation)
		(function res2ref~get-refutation-graph-from-proof-instance)
		(defaults (omega*current-resolution-proof))
		(log-p T)
		(help "Transform the resolution proof into a refutation graph."))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;
(com~deffragment global-verbalization
		 (uses-comms-of direct-display)
		 (help "Commands for the global verbalization"))

(com~defcommand get-textbook-style-verbalization
		(function mverb~get-textbook-style-verbalization)
		(argnames)
		(argtypes)
		(arghelps)
		(frag-cats presentation global-verbalization)
		(help ""))

(com~defcommand get-construction-style-verbalization
		(function mverb~get-construction-style-verbalization)
		(argnames)
		(argtypes)
		(arghelps)
		(frag-cats presentation global-verbalization)
		(help ""))

(com~defcommand get-detailed-verbalization
		(function mverb~get-detailed-verbalization)
		(argnames node1)
		(argtypes ndline)
		(arghelps "")
		(frag-cats presentation)
		(help ""))

(com~defcommand get-local-verbalization
		(function mverb~get-local-verbalization)
		(argnames node1)
		(argtypes ndline)
		(arghelps "")
		(frag-cats presentation)
		(help ""))

(com~defcommand get-lml
		(function mverb~get-local-verbalization)
		(argnames node1)
		(argtypes ndline)
		(arghelps "")
		(frag-cats presentation)
		(help ""))

(com~defcommand verbalization-back
		(function mverb~back)
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats presentation)
		(help""))

(com~defcommand verbalization-forward
		(function mverb~forward)
		(argnames )
		(argtypes )
		(arghelps )
		(frag-cats presentation)
		(help""))

#+(com~defcommand get-lml-for-constraint-state
		(function mverb~parse-constraint-store)
		(argnames node1)
		(argtypes anything)
		(arghelps "")
		(frag-cats presentation)
		(help ""))
