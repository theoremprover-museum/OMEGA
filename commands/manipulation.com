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

(com~defcommand island
  (argnames formula support-list supported-list)
  (argtypes formula ndline-list ndline-list)
  (arghelps "A forumula for a new open node."
	    "A list of support-nodes for the new open-node."
	    "A list of open nodes which are supported by the new open-node.")
  (frag-cats omega-basic proof-manipulation)
  (function oc=insert-island!)
  (defaults ((com~unspecified)
	     (com~unspecified)
	     (com~unspecified)
	     ))
  (log-p T) 
  (help "Introduces a new open line as an island into the plan."))

(com~defcommand local-def-intro
  (argnames term)
  (argtypes term)
  (arghelps "a term that should be used as definiens")
  (function oc=local-def-intro!)
  (defaults ((com~unspecified)
	     ))
  (frag-cats omega-basic proof-manipulation)
  (log-p T)
  (help "Introduces a new local definition."))

