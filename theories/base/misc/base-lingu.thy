;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1993 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     KEIM Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
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





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MIC Part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :keim)

(lex~def-entry agatha
 ((class name)
  (verbalizations ("Agatha" 1))))

(um~def-variable agatha umv@function
  ((arity 0) (priority (verb . 10))
   (um 'um@object :object (lex 'agatha))))


(lex~def-entry charles
 ((class name)
  (verbalizations ("Charles" 1))))

(um~def-variable charles umv@function
  ((arity 0) (priority (verb . 10))
   (um 'um@object :object (lex 'charles))))


(lex~def-entry butler
 ((class noun)
  (verbalizations ("butler" 1))))

(um~def-variable butler umv@function
  ((arity 0) (priority (verb . 10))
   (um 'um@object :object (lex 'butler)
       :annotations '((specifier definite)))))


(lex~def-entry HATES
 ((class VERB)
  (verbalizations ("hate" 1))))

(um~def-variable HATES umv@predicate
  ((arity 2) (priority (verb . 10)) 
   (um 'um@PROCESS-RELATION :process (lex 'HATES) :domain ?1 :range ?2)))

(pr2tag~role-translation "hate" (:domain . agent) (:range . :patient))


(lex~def-entry LIVING
 ((class adjective)
  (verbalizations ("living" 1))))

(um~def-variable lives umv@predicate
  ((arity 1) (priority (verb . 10))
   (um 'um@property-ascription :attribute (lex 'living)
       :attribuend ?1)))


(lex~def-entry KILLED
 ((class VERB)
  (verbalizations ("kill" 1))))

(um~def-variable killed umv@predicate
  ((arity 2) (priority (verb . 10)) 
   (um 'um@PROCESS-RELATION :process (lex 'killed) :domain ?1 :range ?2
       :annotations '((tense imperfekt)))))

(pr2tag~role-translation "kill" (:domain . agent) (:range . :patient))


(lex~def-entry RICH
 ((class adjective)
  (verbalizations ("rich" 1))))

(um~def-variable richer umv@predicate
  ((arity 2) (priority (verb . 10))
   (um 'um@scaled-comparision :attribute (lex 'rich) :attribuend ?1 :comparand ?2)))

;(pr2tag~role-translation "be-richer-than" (:domain . agent) (:range . :patient))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAG Part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :tag)

(ieda 'n "butler" '("" N-SPECIFIER-MODIFIER
 		     (OBL (FUNC SPECIFIER) (REAL BOTH))
 		     (OBL (FUNC N) (person 3) (cat N))
                     (fac (FUNC MODIFIER))))


(ieda 'V "hate" '(""
                    ((FORM (CONTINUOUS VALUE CONT-ACCOBJ (OBL SUBJ $))
                           (PART-1 VALUE PART-ACCOBJ (OBL SUBJ $))
                           (PART-2 VALUE PART (OBL SUBJ $) (OBL ACCOBJ $))
                           (ANY (VOICE (AKTIV VALUE V-SUBJ-ACCOBJ)
                                       (PASSIV VALUE V-SUBJ (OBL SUBJ $) (OBL ACCOBJ SUBJ))))))
                    (OBL (func SUBJ) (real NP))
                    (OBL (func ACCOBJ) (real NP))))



(ieda 'adj "living" '("" adj))

(ieda 'adj "rich" '("" adj))

(ieda 'V "kill" '(""
                    ((FORM (CONTINUOUS VALUE CONT-ACCOBJ (OBL SUBJ $))
                           (PART-1 VALUE PART-ACCOBJ (OBL SUBJ $))
                           (PART-2 VALUE PART (OBL SUBJ $) (OBL ACCOBJ $))
                           (ANY (VOICE (AKTIV VALUE V-SUBJ-ACCOBJ)
                                       (PASSIV VALUE V-SUBJ (OBL SUBJ $) (OBL ACCOBJ SUBJ))))))
                    (OBL (func SUBJ) (real NP))
                    (OBL (func ACCOBJ) (real NP))))




(ieda 'V "be-richer-than" '(""
                    ((FORM (CONTINUOUS VALUE CONT-ACCOBJ (OBL SUBJ $))
                           (PART-1 VALUE PART-ACCOBJ (OBL SUBJ $))
                           (PART-2 VALUE PART (OBL SUBJ $) (OBL ACCOBJ $))
                           (ANY (VOICE (AKTIV VALUE V-SUBJ-ACCOBJ)
                                       (PASSIV VALUE V-SUBJ (OBL SUBJ $) (OBL ACCOBJ SUBJ))))))
                    (OBL (func SUBJ) (real NP))
                    (OBL (func ACCOBJ) (real NP))))
(setq $english-inflection-data$
       (append $english-inflection-data$ '(("be-richer-than"
          (indikative (aktive (praesens (sg (1 . "am richer than") (2 . "are richer than")
					    (3 . "is richer than"))
                                        (pl (1 . "are richer than") (2 . "are richer than")
					    (3 . "are richer than")))))))))
(setq $english-present-participle-inflection$
      (append $english-present-participle-inflection$
	      '(("be-richer-than" "being richer than"))))



;; Don't change the following line until AF or I modified the
;; loading of linguistic data. MP
(in-package :omega)
