;;; -*- syntax: common-lisp; package: OMEGA; base: 10; mode: LISP -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                        ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Bau 36, 4. Stock                                                     ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
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

;;;This file defines the OMEGA black-box inferences  

(infer~defbbox otter
	       (outline-function atp~otter-compute-otter-just) 
	       (test-function )
	       (expansion-function atp~otter-bbox-expansion-function) 
	       (parameter-types boolean)
	       (help "Prove by OTTER"))

;; Propositional logic ATP -> momentan durch PROTEIN abgedeckt
(infer~defbbox pl-atp
	       (outline-function atp~pl-atp-compute-just) 
	       (test-function )
	       (expansion-function atp~pl-atp-bbox-expansion-function) 
	       (parameter-types boolean)
	       (help "Prove by pl-atp"))


(infer~defbbox spass
	       (outline-function atp~spass-compute-spass-just) 
	       (test-function )
	       (expansion-function atp~spass-bbox-expansion-function)
	       (parameter-types boolean)
	       (help "Prove by SPASS"))

(infer~defbbox protein
	       (outline-function atp~protein-compute-protein-just) 
	       (test-function )
	       (expansion-function atp~protein-bbox-expansion-function)
	       (parameter-types boolean)
	       (help "Prove by PROTEIN"))

(infer~defbbox satchmo
	       (outline-function atp~satchmo-compute-satchmo-just) 
	       (test-function )
	       (expansion-function atp~satchmo-bbox-expansion-function)
	       (parameter-types boolean)
	       (help "Prove by SATCHMO"))


(infer~defbbox tps     ;; A black box just for TPS 
	       (outline-function tps=compute-justification-content)
	       (test-function )
	       (expansion-function tps=expand-tps-justified-node)
	       (parameter-types SYMBOL)
	       (help "Proved by TPS"))


(infer~defbbox leo     ;; A black box just for LEO 
	       (outline-function leo=compute-justification-content)
	       (test-function )
	       (expansion-function )
	       (parameter-types )
	       (help "Proved by LEO"))

(infer~defbbox assertion
	       (outline-function altac=outline)
	       (test-function)
	       (expansion-function altac=expand-function)
	       (parameter-types)
	       (help "Assertion level."))

(infer~defbbox bliksem
	       (outline-function atp~bliksem-compute-bliksem-just) 
	       (test-function )
	       (expansion-function atp~bliksem-bbox-expansion-function) 
	       (parameter-types boolean)
	       (help "Prove by BLIKSEM"))

(infer~defbbox eqp
	       (outline-function atp~eqp-compute-eqp-just) 
	       (test-function )
	       (expansion-function atp~eqp-bbox-expansion-function) 
	       (parameter-types boolean)
	       (help "Prove by EQP"))

(infer~defbbox waldmeister
	       (outline-function atp~wald-compute-wald-just) 
	       (test-function )
	       (expansion-function atp~wald-bbox-expansion-function) 
	       (parameter-types boolean)
	       (help "Prove by WALDMEISTER"))


(infer~defbbox atp
	       (outline-function atp~atp-compute-atp-just) 
	       (test-function )
	       (expansion-function atp~atp-bbox-expansion-function) 
	       (parameter-types anything)
	       (help "Prove by some ATP"))
