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


(com~defcommand mbase-enter
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats mbase)
  (function oc=mbase-enter)
  (log-p T)
  (help "Enter the MBASE service and redirect every DB-interaction to MBase"))


(com~defcommand mbase-leave
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats mbase)
  (function oc=mbase-leave)
  (log-p T)
  (help "Leave the MBase service and redirect every DB-interaction to the theory-files."))

(com~defcommand mbase-restart
  (argnames )
  (argtypes )
  (arghelps )
  (frag-cats mbase)
  (function oc=mbase-restart)
  (log-p T)
  (help "Restart the MBase service."))

(com~defcommand load-problems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "The name of a theory whose problems ar to be loaded")
  (frag-cats mbase)
  (function th~load-problems)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Load the problems of the given theory."))

(com~defcommand load-theorems         
  (argnames theory-name)
  (argtypes existing-theory)
  (arghelps "The name of a theory whose theorems are to be loaded")
  (frag-cats mbase)
  (function th~load-theorems)
  (defaults ((oc=default-theory)))
  (log-p T)
  (help "Load the theorems of the given theory."))

(com~defcommand import-ass    
  (argnames ass-name)
  (argtypes thy-assumption)
  (arghelps "A name of an assumption to be imported from the problem theory")
  (function oc=import-ass)
  (frag-cats mbase)
  (log-p T)
  (help "Import a new assumption from the problem theory."))

(com~defcommand import-ass*      
  (argnames ass-list)
  (argtypes thy-ass-list)
  (arghelps "A list of assumptions to be imported from the problem theory")
  (function oc=import-ass*)
  (frag-cats mbase)
  (log-p T)
  (help "Import new assumptions from the problem theory."))

(com~defcommand url-add-path
  (argnames url)
  (argtypes existing-url)
  (arghelps "An URL where you have your local theories, e.g. ''http://host/path/''")
  (function oc=add-theory-path)
  (frag-cats mbase)
  (log-p T)
  (help "Add an URL (or path) to the theory-registry."))

(com~defcommand url-change-path
  (argnames urls)
  (argtypes existing-url-list)
  (arghelps "A list with the path/URL where you have your theories, e.g. ''http://host/path/''")
  (function oc=change-theory-path)
  (defaults oc=show-theory-path)
  (frag-cats mbase)
  (log-p T)
  (help "Change the path/URL of the theory-registry."))
