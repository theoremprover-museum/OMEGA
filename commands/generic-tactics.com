;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: KEIM -*-
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

(com~defcommand assertion
		(argnames premises assertion conclusion)
		(argtypes ndline-or-term-list ndline-or-term ndline-or-term)
		(arghelps "A list of lines or terms that are the premises"
			  "A line or a term that is the assertion"
			  "A line or a term that is the conclusion to be proved")
		(function alift~apply-command!)
		(frag-cats tactics generic)
		(log-p t)
		(help "Tries to justify a conclusion by applying an assertion to premises. If the conclusion, the assertion and/or the premises are terms, new lines are created and inserted into the current proof. If one of the premises is a term, the conclusion must be a line."))


(com~defcommand fully-expand-subproblem
  (argnames concl)
  (argtypes ndline)
  (arghelps  "conclusion line")
  (function oc=fully-expand-subproblem)
  (frag-cats tactics generic) 
  (defaults ((oc~default-current-planline)))
  (log-p T)
  (help "Expands all definitions in the conclusion line and in all of it's hypotheses."))


(com~defcommand expand-subproblem
  (argnames concl supports prohib-defs)
  (argtypes ndline ndline-list thy-ass-list)
  (arghelps  "conclusion line" "A list of support-lines." "A list of definitions prohibited for expansion.")
  (function oc=expand-subproblem)
  (frag-cats tactics generic) 
  (defaults oc=expand-subproblem-defaults)
  (log-p T)
  (help "Expands all definitions -- except those which are explicitly prohibited -- in the conclusion line and in all of it's hypotheses."))

