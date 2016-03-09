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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;    Commands to invoke SAPPER 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(com~defcommand call-cas      
  (argnames line position system)
  (argtypes ndline position symbol)
  (arghelps "A line with term suitable for a CAS" "The position of this term"
	    "The name of a CAS")
  (function cacom~call-cas)
  (frag-cats  extern sapper)
  (defaults cacom~cas-defaults)
  (log-p t)
  (help "Simplify a function with a CAS."))

(com~defcommand suggest-cas     
  (argnames line)
  (argtypes ndline)
  (arghelps "A line to be checked whether a CAS is applicable")
  (function cacom~suggest-cas)
  (frag-cats extern sapper)
  (defaults )
  (log-p nil)
  (help "Get CAS and position where it is applicable for one line."))

(com~defcommand suggest-cas*    
  (argnames line-list)
  (argtypes ndline-list)
  (arghelps "A list of lines to be checked whether a CAS is applicable")
  (function cacom~suggest-cas*)
  (frag-cats extern sapper)
  (defaults )
  (log-p nil)
  (help "Get CAS and position where it is applicable for several lines."))

