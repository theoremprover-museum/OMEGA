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
