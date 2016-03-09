; Chad E Brown; In November 2005 I translated Jutting's Automath version of
; Landau's Grundlagen der Analysis into TPS (without proofs) and then
; from TPS into this post syntax for Omega.  Enjoy!

(th~deftheory landau3
              (uses landau2)
	      (help "Definitions, constants, axioms and problems generated from
Jutting's Automath translation of Landau's analysis book.

Landau Chapter 3"))

(th~defdef _CUTPROP1A (in landau3)
 (definition (_NONEMPTY _RAT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTPROP1B (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (NOT (_RT_ALL (LAM (x1 (O (I I))) (_RT_IN x1 x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTPROP1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (AND (_CUTPROP1A x0) (_CUTPROP1B x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTPROP2A (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RT_ALL (LAM (x2 (O (I I))) (IMPLIES (NOT (_RT_IN x2 x0)) (_RT_LESS x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTPROP2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_RT_ALL (LAM (x1 (O (I I))) (IMPLIES (_RT_IN x1 x0) (_CUTPROP2A x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UBPROP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (IMPLIES (_RT_IN x2 x0) (_RT_MOREIS x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UB (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RT_ALL (_UBPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MAX (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (AND (_UB x0 x1) (_RT_IN x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTPROP3 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (NOT (_RT_SOME (_MAX x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTPROP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (AND (_CUTPROP1 x0) (AND (_CUTPROP2 x0) (_CUTPROP3 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_LBPROP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (IMPLIES (_RT_IN x2 x0) (_RT_LESSIS x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_LB (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RT_ALL (_III1_LBPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_MIN (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (AND (_RT_LB x0 x1) (_RT_IN x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUT (in landau3)
 (definition (_OT (_SET _RAT) _CUTPROP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUT_DEC (in landau3)
 (definition (PER _CUT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LCL (in landau3)
 (definition (LAM (x0 (O (O (I I)))) x0))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LCL_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (_SET _RAT (_LCL x0) (_LCL x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LRT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RT_IN x1 (_LCL x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _URT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (NOT (_RT_IN x1 (_LCL x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLCL (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP (_LCL x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLCL1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP1 (_LCL x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLCL2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP2 (_LCL x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLCL3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP3 (_LCL x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLCL1A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP1A (_LCL x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLCL1B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP1B (_LCL x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTAPP1A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (IMPLIES (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) x1)))) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RT_SOME (_URT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTAPP1B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (IMPLIES (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) x1)))) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (_CUTPROP2A (_LCL x0) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTAPP2A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_LESS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTAPP2B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_MORE x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (NOT (_MAX (_LCL x0) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (NOT (_UB (_LCL x0) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (_RT_SOME (LAM (x2 (O (I I))) (NOT (_UBPROP (_LCL x0) x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_UBPROP (_LCL x0) x1 x4)) (_LRT x0 x4))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_UBPROP (_LCL x0) x1 x4)) (NOT (_RT_MOREIS x1 x4)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_UBPROP (_LCL x0) x1 x4)) (_RT_LESS x1 x4))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_UBPROP (_LCL x0) x1 x4)) x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTAPP3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x1 x3) x2))))) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (_RT_IN x2 x0)) (_CUTPROP1 x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (NOT (_MAX x0 x1))))) (_CUTPROP3 x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (_RT_IN x2 x0)) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (_RT_LESS x3 x4))))))))) (IMPLIES (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (NOT (_MAX x0 x5))))) (_CUTPROP x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_IS (in landau3)
 (definition _CUT)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NIS (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (NOT (_RP_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISE (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (_SET _RAT (_LCL x0) (_LCL x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISE1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_LRT x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISI (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_SET _RAT (_LCL x0) (_LCL x1)) (_RP_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISI1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_LRT x1 x2))))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (_LRT x0 x3))))) (_RP_IS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTOF (in landau3)
 (definition (_OUT (_SET _RAT) _CUTPROP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUTOF_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x1) (IMPLIES (_CUTPROP x0) (_CUT (_CUTOF x0) (_CUTOF x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INE (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (_CUTPROP x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_LRT (_CUTOF x0) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INI (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (_CUTPROP x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_CUTOF x0) x1) (_RT_IN x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISI2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _RAT x1 x1) (IMPLIES (_CUTPROP x0) (IMPLIES (_CUTPROP x1) (IMPLIES (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 x0) (_RT_IN x2 x1))))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x1) (_RT_IN x3 x0))))) (_RP_IS (_CUTOF x0) (_CUTOF x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ALL (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_SOME (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (EXISTS (LAM (x1 (O (O (I I)))) (AND (_CUT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ONE (in landau3)
 (definition (_E_ONE _CUT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ116 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS x0 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ117 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (_RP_IS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ118 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x1 x2) (_RP_IS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1119_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (NOT (_RT_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ119 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 x1) (_URT x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ119A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x1 x2) (_URT x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1120_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (NOT (_RT_MORE x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ120 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_LRT x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ120A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x1 x2) (_LRT x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (IMPLIES (_RT_LESS x4 x3) (_RT_IN x4 x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (NOT (_RT_LESS x4 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (_RT_MOREIS x4 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (IMPLIES (_RT_IS x4 x3) (_RT_IN x4 x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (_RT_NIS x4 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (_RT_MORE x4 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (NOT (_RT_IN x4 x0)) (_RT_LESS x3 x4))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 x0))))))))) (_CUTPROP2 x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (_RT_SOME (LAM (x4 (O (I I))) (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)) (_RT_IN x4 x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)) (_RT_MORE x4 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)) (NOT (_RT_LESSIS x4 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)) (NOT (_RT_MOREIS x3 x4)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)) (NOT (_UBPROP x0 x3 x4)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)) (NOT (_UB x0 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_IN x4 x0) (_RT_MORE x4 x3)) (NOT (_MAX x0 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T28 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (NOT (_MAX x0 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T29 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (NOT (_RT_IN x3 x0)) (NOT (_MAX x0 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T30 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (NOT (_MAX x0 x3))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III1_T31 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (IMPLIES (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 x0) (_RT_MORE x2 x1)))))))) (_CUTPROP3 x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CUT2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (_RT_IN x2 x0)) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 x0) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_LESS x4 x3) (_RT_IN x4 x0))))))))) (IMPLIES (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_IN x5 x0) (_RT_SOME (LAM (x6 (O (I I))) (AND (_RT_IN x6 x0) (_RT_MORE x6 x5)))))))) (_CUTPROP x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_MORE (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_RT_SOME (LAM (x2 (O (I I))) (AND (_LRT x0 x2) (_URT x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III2_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_LRT x0 x4) (_URT x1 x4)) (_LRT x0 x4))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III2_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_LRT x0 x4) (_URT x1 x4)) (_URT x1 x4))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III2_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_LRT x0 x4) (_URT x1 x4)) x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) x2))))) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_LESS (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_RT_SOME (LAM (x2 (O (I I))) (AND (_URT x0 x2) (_LRT x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III2_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_URT x0 x4) (_LRT x1 x4)) (_URT x0 x4))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III2_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_URT x0 x4) (_LRT x1 x4)) (_LRT x1 x4))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III2_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_URT x0 x4) (_LRT x1 x4)) x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) x2))))) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2121_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (AND (_URT x1 x2) (_LRT x0 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2121_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (_RP_LESS x1 x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ121 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_LESS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2122_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (AND (_LRT x1 x2) (_URT x0 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2122_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (_RP_MORE x1 x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ122 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (_RP_MORE x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (NOT (_SET _RAT (_LCL x0) (_LCL x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (NOT (_RP_IS x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (NOT (_RP_IS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (NOT (_RP_MORE x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (NOT (_SET _RAT (_LCL x0) (_LCL x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (NOT (_RP_IS x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (NOT (_RP_IS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (NOT (_RP_IS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) (_RT_LESS x2 x3))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) (_RT_MORE x2 x3))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) FALSE)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) FALSE))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_LESS x0 x1) FALSE)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (NOT (_RP_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (NOT (_RP_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (AND (IMPLIES (_RP_IS x0 x1) (NOT (_RP_MORE x0 x1))) (AND (IMPLIES (_RP_MORE x0 x1) (NOT (_RP_LESS x0 x1))) (IMPLIES (_RP_LESS x0 x1) (NOT (_RP_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (OR (_RP_IS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_NIS x0 x1) (NOT (_SET _RAT (_LCL x0) (_LCL x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_NIS x0 x1) (OR (_RP_MORE x0 x1) (_RP_MORE x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_NIS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_NIS x0 x1) (OR (_RP_IS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2123_B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (OR (_RP_IS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (AND (OR (_RP_IS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1))) (AND (IMPLIES (_RP_IS x0 x1) (NOT (_RP_MORE x0 x1))) (AND (IMPLIES (_RP_MORE x0 x1) (NOT (_RP_LESS x0 x1))) (IMPLIES (_RP_LESS x0 x1) (NOT (_RP_IS x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (OR (_RP_IS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (AND (IMPLIES (_RP_IS x0 x1) (NOT (_RP_MORE x0 x1))) (AND (IMPLIES (_RP_MORE x0 x1) (NOT (_RP_LESS x0 x1))) (IMPLIES (_RP_LESS x0 x1) (NOT (_RP_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_MOREIS (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (OR (_RP_MORE x0 x1) (_RP_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_LESSIS (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (OR (_RP_LESS x0 x1) (_RP_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ124 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MOREIS x0 x1) (_RP_LESSIS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ125 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESSIS x0 x1) (_RP_MOREIS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISMORE1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x0 x2) (_RP_MORE x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISMORE2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x2 x0) (_RP_MORE x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISLESS1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESS x0 x2) (_RP_LESS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISLESS2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESS x2 x0) (_RP_LESS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISMOREIS1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MOREIS x0 x2) (_RP_MOREIS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISMOREIS2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MOREIS x2 x0) (_RP_MOREIS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISLESSIS1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESSIS x0 x2) (_RP_LESSIS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISLESSIS2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESSIS x2 x0) (_RP_LESSIS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_MOREISI2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (_RP_MOREIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_LESSISI2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (_RP_LESSIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_MOREISI1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_MOREIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_LESSISI1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (_RP_LESSIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISMORE12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (IMPLIES (_RP_MORE x0 x2) (_RP_MORE x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISLESS12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (IMPLIES (_RP_LESS x0 x2) (_RP_LESS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISMOREIS12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (IMPLIES (_RP_MOREIS x0 x2) (_RP_MOREIS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISLESSIS12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (IMPLIES (_RP_LESSIS x0 x2) (_RP_LESSIS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MOREIS x0 x1) (NOT (_RP_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESSIS x0 x1) (NOT (_RP_MORE x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123E (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (NOT (_RP_MORE x0 x1)) (_RP_LESSIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123F (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (NOT (_RP_LESS x0 x1)) (_RP_MOREIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123G (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (NOT (_RP_LESSIS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123H (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (NOT (_RP_MOREIS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123J (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (NOT (_RP_MOREIS x0 x1)) (_RP_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ123K (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (NOT (_RP_LESSIS x0 x1)) (_RP_MORE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2126_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_LRT x2 x4) (_RT_LESS x3 x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2126_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_LRT x2 x4) (_URT x0 x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2126_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_LRT x2 x4) (AND (_URT x0 x4) (_LRT x2 x4))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2126_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_LRT x2 x4) (_RP_LESS x0 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2126_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_LRT x1 x3) (_RP_LESS x0 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ126 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x1 x2) (_RP_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_TRLESS (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x1 x2) (_RP_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_TRMORE (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MORE x1 x2) (_RP_MORE x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ127A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESS x1 x2) (_RP_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ127B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESSIS x1 x2) (_RP_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ127C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MORE x1 x2) (_RP_MORE x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ127D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MOREIS x1 x2) (_RP_MORE x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2128_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x1 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x1 x2) (_RP_LESSIS x0 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2128_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x1 x2) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESS x1 x2) (_RP_LESSIS x0 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2128_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x1 x2) (IMPLIES (_RP_IS x0 x1) (_RP_LESSIS x0 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2128_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x1 x2) (IMPLIES (_RP_LESS x0 x1) (_RP_LESSIS x0 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ128 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x1 x2) (_RP_LESSIS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_TRLESSIS (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x1 x2) (_RP_LESSIS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_TRMOREIS (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x1 x2) (_RP_MOREIS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUMPROP1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (AND (_LRT x0 x3) (AND (_LRT x1 x4) (_RT_IS x2 (_RT_PL x3 x4))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUMPROP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_RT_SOME (LAM (x3 (O (I I))) (_RT_SOME (_SUMPROP1 x0 x1 x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUM (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_SETOF _RAT (_SUMPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUM_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_SET _RAT (_SUM x0 x2) (_SUM x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_SUMPROP1 x0 x1 x2 x3 x4))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_SOME (_SUMPROP1 x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_SUMPROP x0 x1 x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUM1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_IN x2 (_SUM x0 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) (_SUMPROP x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_SUMPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_SUMPROP1 x0 x1 x2 x6 x7) (_LRT x0 x6)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_SUMPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_SUMPROP1 x0 x1 x2 x6 x7) (_LRT x1 x7)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_SUMPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_SUMPROP1 x0 x1 x2 x6 x7) (_RT_IS x2 (_RT_PL x6 x7))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_SUMPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_SUMPROP1 x0 x1 x2 x6 x7) x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_SUMPROP1 x0 x1 x2 x6)) x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUMAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_SUM x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_PL x5 x6)) (_RT_LESS x5 x2)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_SUM x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_PL x5 x6)) (_RT_LESS x6 x3)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_SUM x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_PL x5 x6)) (_RT_LESS x4 (_RT_PL x2 x3))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_SUM x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_PL x5 x6)) (_RT_NIS x4 (_RT_PL x2 x3))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_SUM x0 x1)) (_RT_NIS x4 (_RT_PL x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ129A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (NOT (_RT_IN (_RT_PL x2 x3) (_SUM x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_LESS x3 (_RT_PL x4 x5))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Z1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (LAM (x5 (O (I I))) (_OV x3 (_RT_PL x4 x5)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Z1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RT_IN x4 (_SUM x0 x2)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RT_LESS x6 x4) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x10 (O (I I))) (FORALL (LAM (x11 (O (I I))) (IMPLIES (_RAT x10 x11) (IMPLIES (_LRT x2 x10) (IMPLIES (_RT_IS x4 (_RT_PL x8 x10)) (_RAT (_Z1 x0 x2 x4 x6 x8 x10) (_Z1 x1 x3 x5 x7 x9 x11))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_LESS (_RT_TS (_Z1 x0 x1 x2 x3 x4 x5) (_RT_PL x4 x5)) (_RT_TS _1RT (_RT_PL x4 x5)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_LESS (_Z1 x0 x1 x2 x3 x4 x5) _1RT)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_LESS (_RT_TS x4 (_Z1 x0 x1 x2 x3 x4 x5)) x4)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_LESS (_RT_TS x5 (_Z1 x0 x1 x2 x3 x4 x5)) x5)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_LRT x0 (_RT_TS x4 (_Z1 x0 x1 x2 x3 x4 x5)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_LRT x1 (_RT_TS x5 (_Z1 x0 x1 x2 x3 x4 x5)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_IS (_RT_PL (_RT_TS x4 (_Z1 x0 x1 x2 x3 x4 x5)) (_RT_TS x5 (_Z1 x0 x1 x2 x3 x4 x5))) x3)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_IS x3 (_RT_PL (_RT_TS x4 (_Z1 x0 x1 x2 x3 x4 x5)) (_RT_TS x5 (_Z1 x0 x1 x2 x3 x4 x5))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) (_RT_IN x3 (_SUM x0 x1))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (_RT_IN x3 (_SUM x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_IN (_RT_PL x5 x4) (_SUM x0 x1)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE (_RT_PL x5 x4) (_RT_PL x3 x4)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE (_RT_PL x5 x4) x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (AND (_RT_IN (_RT_PL x5 x4) (_SUM x0 x1)) (_RT_MORE (_RT_PL x5 x4) x2)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_SOME (LAM (x6 (O (I I))) (AND (_RT_IN x6 (_SUM x0 x1)) (_RT_MORE x6 x2)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_SOME (LAM (x5 (O (I I))) (AND (_RT_IN x5 (_SUM x0 x1)) (_RT_MORE x5 x2))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_SUM x0 x1)) (_RT_SOME (LAM (x3 (O (I I))) (AND (_RT_IN x3 (_SUM x0 x1)) (_RT_MORE x3 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (_CUTPROP (_SUM x0 x1)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (_CUTPROP (_SUM x0 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (_CUTPROP (_SUM x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3129_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_CUTPROP (_SUM x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ129 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_CUTPROP (_SUM x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_PL (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_CUTOF (_SUM x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_PL_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_CUT (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LRTPL (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_LRT (_RP_PL x0 x1) x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (NOT (_RT_IN x2 (_SUM x0 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _URTPL (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_URT (_RP_PL x0 x1) x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x0 x1) x2) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) (_RT_IN x2 (_SUM x0 x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PLAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x0 x1) x2) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_PL x4 x5)) x3))))))))) x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISPL1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_PL x0 x2) (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISPL2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_PL x2 x0) (_RP_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISPL12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (_RP_IS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3130_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x0 x1) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_IS x2 (_RT_PL x4 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3130_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x0 x1) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_LRT (_RP_PL x1 x0) x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3130_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x0 x1) x2) (_LRT (_RP_PL x1 x0) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ130 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS (_RP_PL x0 x1) (_RP_PL x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_COMPL (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS (_RP_PL x0 x1) (_RP_PL x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_PL x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_PL x6 x7)) (_RT_IS x3 (_RT_PL x6 (_RT_PL x7 x5)))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_PL x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_PL x6 x7)) (_LRT (_RP_PL x1 x2) (_RT_PL x7 x5))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_PL x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_PL x6 x7)) (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_PL x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3) (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_PL x6 x7)) (_RT_IS x3 (_RT_PL (_RT_PL x4 x6) x7))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_PL x6 x7)) (_LRT (_RP_PL x0 x1) (_RT_PL x4 x6))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_PL x6 x7)) (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3131_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL x0 (_RP_PL x1 x2)) x3) (_LRT (_RP_PL (_RP_PL x0 x1) x2) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ131 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_PL (_RP_PL x0 x1) x2) (_RP_PL x0 (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ASSPL1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_PL (_RP_PL x0 x1) x2) (_RP_PL x0 (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ASSPL2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_PL x0 (_RP_PL x1 x2)) (_RP_PL (_RP_PL x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_PROP1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (AND (_LRT x0 x2) (_URT x0 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_3132_PROP1 x0 x1 x2 x3) (_RT_MORE x3 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_PROP2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (_RT_IS (_RT_MN x3 x2) x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_PROP3 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (AND (_3132_PROP1 x0 x1 x2 x3) (IMPLIES (_3132_PROP1 x0 x1 x2 x3) (_3132_PROP2 x0 x1 x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_PROP4 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RT_SOME (LAM (x2 (O (I I))) (_RT_SOME (_3132_PROP3 x0 x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (_RT_MORE x3 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_RT_MORE (_RT_TS (_RTOFN x4) x1) (_RT_MN x3 x2)) (_RT_MORE (_RT_PL x2 (_RT_TS (_RTOFN x4) x1)) (_RT_PL x2 (_RT_MN x3 x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_RT_MORE (_RT_TS (_RTOFN x4) x1) (_RT_MN x3 x2)) (_RT_MORE (_RT_PL x2 (_RT_TS (_RTOFN x4) x1)) x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_RT_MORE (_RT_TS (_RTOFN x4) x1) (_RT_MN x3 x2)) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x4) x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_RT_MORE (_RT_TS (_RTOFN x4) x1) (_RT_MN x3 x2)) (_N_SOME (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (_N_SOME (LAM (x4 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x4) x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (_LB (LAM (x6 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x6) x1)))) x4)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x4) x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_U0 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 I) (_RT_PL x2 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_U0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_URT x0 x6) (FORALL (LAM (x8 I) (FORALL (LAM (x9 I) (IMPLIES (= x8 x9) (IMPLIES (_MIN (LAM (x10 I) (_URT x0 (_RT_PL x4 (_RT_TS (_RTOFN x10) x2)))) x8) (IMPLIES (= x8 _1) (_RAT (_3132_U0 x0 x2 x4 x6 x8) (_3132_U0 x1 x3 x5 x7 x9))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (= x4 _1) (_RT_IS (_RT_TS (_RTOFN x4) x1) x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (= x4 _1) (_URT x0 (_3132_U0 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (= x4 _1) (_3132_PROP1 x0 x1 x2 (_3132_U0 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (= x4 _1) (IMPLIES (_3132_PROP1 x0 x1 x2 (_3132_U0 x0 x1 x2 x3 x4)) (_3132_PROP2 x0 x1 x2 (_3132_U0 x0 x1 x2 x3 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (= x4 _1) (_3132_PROP3 x0 x1 x2 (_3132_U0 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (= x4 _1) (_RT_SOME (_3132_PROP3 x0 x1 x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (= x4 _1) (_3132_PROP4 x0 x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_RT_MORE (_RTOFN x4) _1RT))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UM10 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 I) (_RT_MN (_RTOFN x4) _1RT)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UM10_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_URT x0 x6) (FORALL (LAM (x8 I) (FORALL (LAM (x9 I) (IMPLIES (= x8 x9) (IMPLIES (_MIN (LAM (x10 I) (_URT x0 (_RT_PL x4 (_RT_TS (_RTOFN x10) x2)))) x8) (IMPLIES (_MORE x8 _1) (_RAT (_UM10 x0 x2 x4 x6 x8) (_UM10 x1 x3 x5 x7 x9))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_NATRT (_UM10 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UM1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 I) (_NOFRT (_UM10 x0 x1 x2 x3 x4))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UM1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_URT x0 x6) (FORALL (LAM (x8 I) (FORALL (LAM (x9 I) (IMPLIES (= x8 x9) (IMPLIES (_MIN (LAM (x10 I) (_URT x0 (_RT_PL x4 (_RT_TS (_RTOFN x10) x2)))) x8) (IMPLIES (_MORE x8 _1) (= (_UM1 x0 x2 x4 x6 x8) (_UM1 x1 x3 x5 x7 x9))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _V0 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 I) (_RT_PL x2 (_RT_TS (_UM10 x0 x1 x2 x3 x4) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _V0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_URT x0 x6) (FORALL (LAM (x8 I) (FORALL (LAM (x9 I) (IMPLIES (= x8 x9) (IMPLIES (_MIN (LAM (x10 I) (_URT x0 (_RT_PL x4 (_RT_TS (_RTOFN x10) x2)))) x8) (IMPLIES (_MORE x8 _1) (_RAT (_V0 x0 x2 x4 x6 x8) (_V0 x1 x3 x5 x7 x9))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _W0 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 I) (_RT_PL x2 (_RT_TS (_RTOFN x4) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _W0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_URT x0 x6) (FORALL (LAM (x8 I) (FORALL (LAM (x9 I) (IMPLIES (= x8 x9) (IMPLIES (_MIN (LAM (x10 I) (_URT x0 (_RT_PL x4 (_RT_TS (_RTOFN x10) x2)))) x8) (IMPLIES (_MORE x8 _1) (_RAT (_W0 x0 x2 x4 x6 x8) (_W0 x1 x3 x5 x7 x9))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_RT_LESS (_UM10 x0 x1 x2 x3 x4) (_RTOFN x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_LESSF (_FR (_UM1 x0 x1 x2 x3 x4) _1) (_FR x4 _1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_LESS (_UM1 x0 x1 x2 x3 x4) x4))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (NOT (_LESSIS x4 (_UM1 x0 x1 x2 x3 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_LRT x0 (_RT_PL x2 (_RT_TS (_RTOFN (_UM1 x0 x1 x2 x3 x4)) x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_LRT x0 (_V0 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_3132_PROP1 x0 x1 (_V0 x0 x1 x2 x3 x4) (_W0 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_RT_IS (_RT_PL (_RT_TS (_UM10 x0 x1 x2 x3 x4) x1) x1) (_RT_TS (_RTOFN x4) x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_RT_IS (_RT_PL (_V0 x0 x1 x2 x3 x4) x1) (_W0 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T28 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (IMPLIES (_3132_PROP1 x0 x1 (_V0 x0 x1 x2 x3 x4) (_W0 x0 x1 x2 x3 x4)) (_3132_PROP2 x0 x1 (_V0 x0 x1 x2 x3 x4) (_W0 x0 x1 x2 x3 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T29 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_3132_PROP3 x0 x1 (_V0 x0 x1 x2 x3 x4) (_W0 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T30 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_RT_SOME (_3132_PROP3 x0 x1 (_V0 x0 x1 x2 x3 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T31 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (IMPLIES (_MORE x4 _1) (_3132_PROP4 x0 x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T32 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_MIN (LAM (x5 I) (_URT x0 (_RT_PL x2 (_RT_TS (_RTOFN x5) x1)))) x4) (_3132_PROP4 x0 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T34 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (_3132_PROP4 x0 x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T35 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_3132_PROP4 x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ132 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_SOME (LAM (x2 (O (I I))) (_RT_SOME (LAM (x3 (O (I I))) (AND (AND (_LRT x0 x2) (_URT x0 x3)) (IMPLIES (AND (_LRT x0 x2) (_URT x0 x3)) (_RT_IS (_RT_MN x3 x2) x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T36 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_3132_PROP3 x0 x2 x5 x6) (_3132_PROP1 x0 x2 x5 x6)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T37 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_3132_PROP3 x0 x2 x5 x6) (_3132_PROP2 x0 x2 x5 x6)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T38 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_3132_PROP3 x0 x2 x5 x6) (_LRT x0 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T39 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_3132_PROP3 x0 x2 x5 x6) (_URT x0 x6)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T40 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_3132_PROP3 x0 x2 x5 x6) (_RT_IS (_RT_MN x6 x5) (_RT_MN x6 x5))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T41 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_3132_PROP3 x0 x2 x5 x6) (_RT_IS (_RT_MN x6 x5) x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T42 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_3132_PROP3 x0 x2 x5 x6) x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3132_T43 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RT_SOME (_3132_PROP3 x0 x2 x5)) x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ132APP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 O) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) x1))))))))) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3133_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) (_RT_IS x4 (_RT_PL x3 x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3133_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) (_LRT (_RP_PL x0 x1) x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3133_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) (AND (_LRT (_RP_PL x0 x1) x4) (_URT x0 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3133_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) x2) (_RP_MORE (_RP_PL x0 x1) x0)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3133_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x1 x2) (_RP_MORE (_RP_PL x0 x1) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ133 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_MORE (_RP_PL x0 x1) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ133A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_LESS x0 (_RP_PL x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (_URT x1 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (_RT_MORE x4 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x2 x6) (IMPLIES (_RT_IS (_RT_MN x6 x5) (_RT_MN x4 x3)) (_RT_IS x6 (_RT_PL (_RT_MN x4 x3) x5))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x2 x6) (IMPLIES (_RT_IS (_RT_MN x6 x5) (_RT_MN x4 x3)) (_RT_IS (_RT_PL x3 x6) (_RT_PL x4 x5))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x2 x6) (IMPLIES (_RT_IS (_RT_MN x6 x5) (_RT_MN x4 x3)) (_LRT (_RP_PL x0 x2) (_RT_PL x3 x6))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x2 x6) (IMPLIES (_RT_IS (_RT_MN x6 x5) (_RT_MN x4 x3)) (_URT (_RP_PL x1 x2) (_RT_PL x3 x6))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x2 x6) (IMPLIES (_RT_IS (_RT_MN x6 x5) (_RT_MN x4 x3)) (AND (_LRT (_RP_PL x0 x2) (_RT_PL x3 x6)) (_URT (_RP_PL x1 x2) (_RT_PL x3 x6)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x2 x6) (IMPLIES (_RT_IS (_RT_MN x6 x5) (_RT_MN x4 x3)) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x3 x4) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3134_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ134 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_PL x0 x2) (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (_RP_MORE (_RP_PL x2 x0) (_RP_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135E (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_PL x2 x0) (_RP_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135F (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (_RP_LESS (_RP_PL x2 x0) (_RP_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135G (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135H (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_PL x2 x0) (_RP_PL x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135J (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ135K (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_PL x2 x0) (_RP_PL x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3136_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (OR (_RP_IS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3136_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (AND (IMPLIES (_RP_IS (_RP_PL x0 x2) (_RP_PL x1 x2)) (NOT (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2)))) (AND (IMPLIES (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2)) (NOT (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x2)))) (IMPLIES (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x2)) (NOT (_RP_IS (_RP_PL x0 x2) (_RP_PL x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ136A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2)) (_RP_MORE x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ136B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_PL x0 x2) (_RP_PL x1 x2)) (_RP_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ136C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x2)) (_RP_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ136D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE (_RP_PL x2 x0) (_RP_PL x2 x1)) (_RP_MORE x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ136E (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_PL x2 x0) (_RP_PL x2 x1)) (_RP_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ136F (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS (_RP_PL x2 x0) (_RP_PL x2 x1)) (_RP_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3137_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3137_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_PL x1 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ137 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ137A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ138A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ138B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (_RP_MORE (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ138C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ138D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESSIS x2 x3) (_RP_LESS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3139_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (_RP_MOREIS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3139_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MOREIS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3139_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_IS x0 x1) (_RP_MOREIS (_RP_PL x0 x2) (_RP_PL x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3139_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_MORE x0 x1) (_RP_MOREIS (_RP_PL x0 x2) (_RP_PL x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ139 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (_RP_MOREIS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ139A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x2 x3) (_RP_LESSIS (_RP_PL x0 x2) (_RP_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESSIS x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_PL x1 x2) x0) (_RP_MORE x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESSIS x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_NIS (_RP_PL x1 x2) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _VORBEMERKUNG140 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESSIS x0 x1) (NOT (_RP_SOME (LAM (x2 (O (O (I I)))) (_RP_IS (_RP_PL x1 x2) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_PL x1 x2) (_RP_PL x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x2 x3) (_RP_NIS (_RP_PL x1 x2) (_RP_PL x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_PL x1 x2) (_RP_PL x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x2 x3) (_RP_NIS (_RP_PL x1 x2) (_RP_PL x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_NIS x2 x3) (OR (_RP_MORE x2 x3) (_RP_LESS x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_NIS x2 x3) (_RP_NIS (_RP_PL x1 x2) (_RP_PL x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS (_RP_PL x1 x2) x0) (IMPLIES (_RP_IS (_RP_PL x1 x3) x0) (_RP_IS x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFFPROP1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (AND (_RT_MORE x3 x4) (IMPLIES (_RT_MORE x3 x4) (_RT_IS x2 (_RT_MN x3 x4))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFFPROP2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (AND (_LRT x0 x3) (AND (_URT x1 x4) (_DIFFPROP1 x0 x1 x2 x3 x4)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_DIFFPROP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_RT_SOME (LAM (x3 (O (I I))) (_RT_SOME (_DIFFPROP2 x0 x1 x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFF (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_SETOF _RAT (_RP_DIFFPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFF_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_SET _RAT (_DIFF x0 x2) (_DIFF x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T11A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (IMPLIES (_RT_MORE x3 x4) (_RT_IS x2 (_RT_MN x3 x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (_DIFFPROP1 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (_DIFFPROP2 x0 x1 x2 x3 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (_RT_SOME (_DIFFPROP2 x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (_RP_DIFFPROP x0 x1 x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFF1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (_RT_IN x2 (_DIFF x0 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (_RP_DIFFPROP x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_DIFFPROP2 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_DIFFPROP2 x0 x1 x2 x6 x7) (_LRT x0 x6)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_DIFFPROP2 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_DIFFPROP2 x0 x1 x2 x6 x7) (_URT x1 x7)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_DIFFPROP2 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_DIFFPROP2 x0 x1 x2 x6 x7) (_DIFFPROP1 x0 x1 x2 x6 x7)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_DIFFPROP2 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_DIFFPROP2 x0 x1 x2 x6 x7) (_RT_MORE x6 x7)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_DIFFPROP2 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_DIFFPROP2 x0 x1 x2 x6 x7) (_RT_IS x2 (_RT_MN x6 x7))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_DIFFPROP2 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_DIFFPROP2 x0 x1 x2 x6 x7) x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III3_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_DIFFPROP2 x0 x1 x2 x6)) x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFFAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) x3)))))))))) x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (_RT_MORE x3 x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (_RT_IN (_RT_MN x3 x2) (_DIFF x0 x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 (_DIFF x0 x1)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x3 (_RT_MN x4 x5)) (_RT_LESS x3 x4)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 (_DIFF x0 x1)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x3 (_RT_MN x4 x5)) (_RT_LESS x3 x2)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 (_DIFF x0 x1)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x3 (_RT_MN x4 x5)) (_RT_NIS x3 x2)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IN x3 (_DIFF x0 x1)) (_RT_NIS x3 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (NOT (_RT_IN x2 (_DIFF x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) (_RT_IS (_RT_PL x2 x5) x4)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (_RT_PL x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X2_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_RP_MORE x0 x2) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RT_IN x4 (_DIFF x0 x2)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RT_LESS x6 x4) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x10 (O (I I))) (FORALL (LAM (x11 (O (I I))) (IMPLIES (_RAT x10 x11) (IMPLIES (_URT x2 x10) (IMPLIES (_RT_MORE x8 x10) (IMPLIES (_RT_IS x4 (_RT_MN x8 x10)) (_RAT (_X2 x0 x2 x4 x6 x8 x10) (_X2 x1 x3 x5 x7 x9 x11))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) (_RT_LESS (_X2 x0 x1 x2 x3 x4 x5) x4)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) (_LRT x0 (_X2 x0 x1 x2 x3 x4 x5))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) (_RT_MORE (_X2 x0 x1 x2 x3 x4 x5) x5)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) (_RT_IS x3 (_RT_MN (_X2 x0 x1 x2 x3 x4 x5) x5))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_MORE x4 x5) (IMPLIES (_RT_IS x2 (_RT_MN x4 x5)) (_RT_IN x3 (_DIFF x0 x1))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (_RT_IN x3 (_DIFF x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE x5 x3))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE x5 x4))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE (_RT_PL (_RT_MN x5 x4) x4) (_RT_PL (_RT_MN x3 x4) x4)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE (_RT_MN x5 x4) (_RT_MN x3 x4)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE (_RT_MN x5 x4) x2))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T28 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_IN (_RT_MN x5 x4) (_DIFF x0 x1)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T29 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (AND (_RT_IN (_RT_MN x5 x4) (_DIFF x0 x1)) (_RT_MORE (_RT_MN x5 x4) x2)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T30 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_SOME (LAM (x6 (O (I I))) (AND (_RT_IN x6 (_DIFF x0 x1)) (_RT_MORE x6 x2)))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T31 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_MORE x3 x4) (IMPLIES (_RT_IS x2 (_RT_MN x3 x4)) (_RT_SOME (LAM (x5 (O (I I))) (AND (_RT_IN x5 (_DIFF x0 x1)) (_RT_MORE x5 x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T32 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_DIFF x0 x1)) (_RT_SOME (LAM (x3 (O (I I))) (AND (_RT_IN x3 (_DIFF x0 x1)) (_RT_MORE x3 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T33 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (_CUTPROP (_DIFF x0 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T34 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (_CUTPROP (_DIFF x0 x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T35 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (_CUTPROP (_DIFF x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140H (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_CUTPROP (_DIFF x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CHI (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_CUTOF (_DIFF x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CHI_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_RP_MORE x0 x2) (_CUT (_CHI x0 x2) (_CHI x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T36 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_CHI x0 x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x1 x6) (IMPLIES (_RT_MORE x5 x6) (IMPLIES (_RT_IS x4 (_RT_MN x5 x6)) (_RT_MORE x6 x3))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T37 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_CHI x0 x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x1 x6) (IMPLIES (_RT_MORE x5 x6) (IMPLIES (_RT_IS x4 (_RT_MN x5 x6)) (_RT_IS (_RT_PL (_RT_PL (_RT_MN x5 x6) x3) (_RT_MN x6 x3)) x5))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T38 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_CHI x0 x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x1 x6) (IMPLIES (_RT_MORE x5 x6) (IMPLIES (_RT_IS x4 (_RT_MN x5 x6)) (_RT_LESS (_RT_PL (_RT_MN x5 x6) x3) x5))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T39 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_CHI x0 x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x1 x6) (IMPLIES (_RT_MORE x5 x6) (IMPLIES (_RT_IS x4 (_RT_MN x5 x6)) (_RT_IS x2 (_RT_PL (_RT_MN x5 x6) x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T40 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_CHI x0 x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x1 x6) (IMPLIES (_RT_MORE x5 x6) (IMPLIES (_RT_IS x4 (_RT_MN x5 x6)) (_RT_LESS x2 x5))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T41 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_CHI x0 x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_URT x1 x6) (IMPLIES (_RT_MORE x5 x6) (IMPLIES (_RT_IS x4 (_RT_MN x5 x6)) (_LRT x0 x2))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T42 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_CHI x0 x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_LRT x0 x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL x1 (_CHI x0 x1)) x2) (_LRT x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T43 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (_RT_MORE x3 x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T44 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (_RT_MORE x5 x4)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T45 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_RT_MORE x2 x4))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T46 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_RT_IS x5 (_RT_PL (_RT_MN x3 x2) x4)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T47 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_RT_IS (_RT_PL x5 (_RT_MN x2 x4)) x3))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_T48 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_RT_MORE x3 x5))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T49 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_RT_IS (_RT_MN x2 x4) (_RT_MN x3 x5)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T50 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_RT_IS x2 (_RT_PL (_RT_MN x3 x5) x4)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T51 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_LRT (_CHI x0 x1) (_RT_MN x3 x5)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T52 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (IMPLIES (_RT_IS (_RT_MN x5 x4) (_RT_MN x3 x2)) (_LRT (_RP_PL x1 (_CHI x0 x1)) x2))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T53 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_RT_LESS x2 x3) (_LRT (_RP_PL x1 (_CHI x0 x1)) x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T54 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_URT x1 x2) (_LRT (_RP_PL x1 (_CHI x0 x1)) x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T55 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (_LRT (_RP_PL x1 (_CHI x0 x1)) x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T56 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (IMPLIES (_URT x1 x3) (_LRT (_RP_PL x1 (_CHI x0 x1)) x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T57 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_LRT x1 x2) (_LRT (_RP_PL x1 (_CHI x0 x1)) x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3140_B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_LRT (_RP_PL x1 (_CHI x0 x1)) x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T58 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RP_PL x1 (_CHI x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_SOME (LAM (x2 (O (O (I I)))) (_RP_IS (_RP_PL x1 x2) x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T59 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_AMONE _CUT (LAM (x2 (O (O (I I)))) (_RP_IS (_RP_PL x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_ONE (LAM (x2 (O (O (I I)))) (_RP_IS (_RP_PL x1 x2) x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_MN (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (THAT (LAM (x2 (O (O (I I)))) (AND (_CUT x2 x2) (_RP_IS (_RP_PL x1 x2) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_MN_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_RP_MORE x0 x2) (_CUT (_RP_MN x0 x2) (_RP_MN x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RP_PL x1 (_RP_MN x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS x0 (_RP_PL x1 (_RP_MN x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140E (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RP_PL (_RP_MN x0 x1) x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140F (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS x0 (_RP_PL (_RP_MN x0 x1) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ140G (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_IS (_RP_PL x1 x2) x0) (_RP_IS x2 (_RP_MN x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T60 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x2) (IMPLIES (_RP_MORE x1 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (_RP_IS (_RP_PL x3 (_RP_MN x0 x2)) x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISMN12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x2) (IMPLIES (_RP_MORE x1 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (_RP_IS (_RP_MN x0 x2) (_RP_MN x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMN1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x2) (IMPLIES (_RP_MORE x1 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_MN x0 x2) (_RP_MN x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMN2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x2 x0) (IMPLIES (_RP_MORE x2 x1) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_MN x2 x0) (_RP_MN x2 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PRODPROP1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (AND (_LRT x0 x3) (AND (_LRT x1 x4) (_RT_IS x2 (_RT_TS x3 x4))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PRODPROP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_RT_SOME (LAM (x3 (O (I I))) (_RT_SOME (_PRODPROP1 x0 x1 x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROD (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_SETOF _RAT (_PRODPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROD_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_SET _RAT (_PROD x0 x2) (_PROD x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_PRODPROP1 x0 x1 x2 x3 x4))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_SOME (_PRODPROP1 x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_PRODPROP x0 x1 x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROD1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_IN x2 (_PROD x0 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) (_PRODPROP x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_PRODPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_PRODPROP1 x0 x1 x2 x6 x7) (_LRT x0 x6)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_PRODPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_PRODPROP1 x0 x1 x2 x6 x7) (_LRT x1 x7)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_PRODPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_PRODPROP1 x0 x1 x2 x6 x7) (_RT_IS x2 (_RT_TS x6 x7))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_PRODPROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_PRODPROP1 x0 x1 x2 x6 x7) x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_PRODPROP1 x0 x1 x2 x6)) x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PRODAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_PROD x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_TS x5 x6)) (_RT_LESS x5 x2)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_PROD x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_TS x5 x6)) (_RT_LESS x6 x3)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_PROD x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_TS x5 x6)) (_RT_LESS x4 (_RT_TS x2 x3))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_PROD x0 x1)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (IMPLIES (_RT_IS x4 (_RT_TS x5 x6)) (_RT_NIS x4 (_RT_TS x2 x3))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RT_IN x4 (_PROD x0 x1)) (_RT_NIS x4 (_RT_TS x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ141A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x1 x3) (NOT (_RT_IN (_RT_TS x2 x3) (_PROD x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_V0 (in landau3)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_RT_TS (_OV _1RT x1) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_V0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_RAT (_4141_V0 x0 x2) (_4141_V0 x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_TS x1 (_4141_V0 x0 x1)) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ141B (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_TS (_OV _1RT x1) x0) (_OV x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ141C (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_OV x0 x1) (_RT_TS (_OV _1RT x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) (_RT_LESS x3 (_RT_TS x4 x5))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) (_RT_IS (_RT_TS (_OV _1RT x4) (_RT_TS x4 x5)) x5)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) (_RT_LESS (_OV x3 x4) x5)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) (_LRT x1 (_OV x3 x4))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) (_RT_IN x3 (_PROD x0 x1))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x3 x2) (_RT_IN x3 (_PROD x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_IN (_RT_TS x5 x4) (_PROD x0 x1)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE (_RT_TS x5 x4) (_RT_TS x3 x4)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_MORE (_RT_TS x5 x4) x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (AND (_RT_IN (_RT_TS x5 x4) (_PROD x0 x1)) (_RT_MORE (_RT_TS x5 x4) x2)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x0 x5) (IMPLIES (_RT_LESS x3 x5) (_RT_SOME (LAM (x6 (O (I I))) (AND (_RT_IN x6 (_PROD x0 x1)) (_RT_MORE x6 x2)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_SOME (LAM (x5 (O (I I))) (AND (_RT_IN x5 (_PROD x0 x1)) (_RT_MORE x5 x2))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IN x2 (_PROD x0 x1)) (_RT_SOME (LAM (x3 (O (I I))) (AND (_RT_IN x3 (_PROD x0 x1)) (_RT_MORE x3 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_URT x1 x5) (_CUTPROP (_PROD x0 x1)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (_CUTPROP (_PROD x0 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (_CUTPROP (_PROD x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4141_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_CUTPROP (_PROD x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ141 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_CUTPROP (_PROD x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_TS (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_CUTOF (_PROD x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_TS_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_CUT (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LRTTS (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_LRT (_RP_TS x0 x1) x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (NOT (_RT_IN x2 (_PROD x0 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _URTTS (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_URT (_RP_TS x0 x1) x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS x0 x1) x2) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) (_RT_IN x2 (_PROD x0 x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TSAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS x0 x1) x2) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x1 x5) (IMPLIES (_RT_IS x2 (_RT_TS x4 x5)) x3))))))))) x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISTS1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_TS x0 x2) (_RP_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISTS2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_TS x2 x0) (_RP_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ISTS12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (_RP_IS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4142_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS x0 x1) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_IS x2 (_RT_TS x4 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4142_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS x0 x1) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x1 x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_LRT (_RP_TS x1 x0) x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4142_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS x0 x1) x2) (_LRT (_RP_TS x1 x0) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ142 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS (_RP_TS x0 x1) (_RP_TS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_COMTS (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS (_RP_TS x0 x1) (_RP_TS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (_RT_IS x3 (_RT_TS x6 (_RT_TS x7 x5)))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (_LRT (_RP_TS x1 x2) (_RT_TS x7 x5))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT x2 x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3) (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_TS x6 x7)) (_RT_IS x3 (_RT_TS (_RT_TS x4 x6) x7))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_TS x6 x7)) (_LRT (_RP_TS x0 x1) (_RT_TS x4 x6))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_TS x6 x7)) (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4143_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_TS x1 x2)) x3) (_LRT (_RP_TS (_RP_TS x0 x1) x2) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ143 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_TS (_RP_TS x0 x1) x2) (_RP_TS x0 (_RP_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ASSTS1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_TS (_RP_TS x0 x1) x2) (_RP_TS x0 (_RP_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_ASSTS2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_TS x0 (_RP_TS x1 x2)) (_RP_TS (_RP_TS x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_PL x6 x7)) (_RT_IS x3 (_RT_PL (_RT_TS x4 x6) (_RT_TS x4 x7)))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_PL x6 x7)) (_LRT (_RP_TS x0 x1) (_RT_TS x4 x6))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_PL x6 x7)) (_LRT (_RP_TS x0 x2) (_RT_TS x4 x7))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x1 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x2 x7) (IMPLIES (_RT_IS x5 (_RT_PL x6 x7)) (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT x0 x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_PL x1 x2) x5) (IMPLIES (_RT_IS x3 (_RT_TS x4 x5)) (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3) (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_RT_IS x3 (_RT_PL (_RT_TS x6 x7) (_RT_TS x8 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_X2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (O (I I)))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (LAM (x5 (O (I I))) (LAM (x6 (O (I I))) (LAM (x7 (O (I I))) (LAM (x8 (O (I I))) (LAM (x9 (O (I I))) (_ITE (_RT_MOREIS x6 x8) _RAT x6 x8))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_X2_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (O (I I)))) (FORALL (LAM (x5 (O (O (I I)))) (IMPLIES (_CUT x4 x5) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x2) (_RP_TS x0 x4)) x6) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (IMPLIES (_LRT (_RP_TS x0 x2) x8) (FORALL (LAM (x10 (O (I I))) (FORALL (LAM (x11 (O (I I))) (IMPLIES (_RAT x10 x11) (IMPLIES (_LRT (_RP_TS x0 x4) x10) (IMPLIES (_RT_IS x6 (_RT_PL x8 x10)) (FORALL (LAM (x12 (O (I I))) (FORALL (LAM (x13 (O (I I))) (IMPLIES (_RAT x12 x13) (IMPLIES (_LRT x0 x12) (FORALL (LAM (x14 (O (I I))) (FORALL (LAM (x15 (O (I I))) (IMPLIES (_RAT x14 x15) (IMPLIES (_LRT x2 x14) (IMPLIES (_RT_IS x8 (_RT_TS x12 x14)) (FORALL (LAM (x16 (O (I I))) (FORALL (LAM (x17 (O (I I))) (IMPLIES (_RAT x16 x17) (IMPLIES (_LRT x0 x16) (FORALL (LAM (x18 (O (I I))) (FORALL (LAM (x19 (O (I I))) (IMPLIES (_RAT x18 x19) (IMPLIES (_LRT x4 x18) (IMPLIES (_RT_IS x10 (_RT_TS x16 x18)) (_RAT (_4144_X2 x0 x2 x4 x6 x8 x10 x12 x14 x16 x18) (_4144_X2 x1 x3 x5 x7 x9 x11 x13 x15 x17 x19)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (_RT_MOREIS x6 x8) (_RT_IS (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) x6)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (_RT_MOREIS x6 x8) (_LRT x0 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (_RT_MOREIS x6 x8) (_RT_LESSIS x6 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (_RT_MOREIS x6 x8) (_RT_LESSIS x8 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (NOT (_RT_MOREIS x6 x8)) (_RT_IS (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) x8)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (NOT (_RT_MOREIS x6 x8)) (_LRT x0 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (NOT (_RT_MOREIS x6 x8)) (_RT_LESSIS x8 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (IMPLIES (NOT (_RT_MOREIS x6 x8)) (_RT_LESSIS x6 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_LRT x0 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_RT_LESSIS x6 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_RT_LESSIS x8 (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_LRT (_RP_PL x1 x2) (_RT_PL x7 x9)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_LRT (_RP_TS x0 (_RP_PL x1 x2)) (_RT_TS (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) (_RT_PL x7 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_RT_LESSIS (_RT_TS x6 x7) (_RT_TS (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) x7)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_RT_LESSIS (_RT_TS x8 x9) (_RT_TS (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) x9)))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_RT_LESSIS x3 (_RT_TS (_4144_X2 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9) (_RT_PL x7 x9))))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_LRT x0 x8) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x9 x9) (IMPLIES (_LRT x2 x9) (IMPLIES (_RT_IS x5 (_RT_TS x8 x9)) (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3))))))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_LRT x0 x6) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_LRT x1 x7) (IMPLIES (_RT_IS x4 (_RT_TS x6 x7)) (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RP_TS x0 x1) x4) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_LRT (_RP_TS x0 x2) x5) (IMPLIES (_RT_IS x3 (_RT_PL x4 x5)) (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4144_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) x3) (_LRT (_RP_TS x0 (_RP_PL x1 x2)) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ144 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_TS x0 (_RP_PL x1 x2)) (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_DISTTP1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_TS (_RP_PL x0 x1) x2) (_RP_PL (_RP_TS x0 x2) (_RP_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_DISTTP2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_TS x0 (_RP_PL x1 x2)) (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_DISTPT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_PL (_RP_TS x0 x2) (_RP_TS x1 x2)) (_RP_TS (_RP_PL x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_DISTPT2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (_RP_IS (_RP_PL (_RP_TS x0 x1) (_RP_TS x0 x2)) (_RP_TS x0 (_RP_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PHI (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (O (I I)))) (_RP_MN x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PHI_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (O (I I)))) (FORALL (LAM (x5 (O (O (I I)))) (IMPLIES (_CUT x4 x5) (IMPLIES (_RP_MORE x0 x2) (_CUT (_PHI x0 x2 x4) (_PHI x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4145_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (_RP_IS x0 (_RP_PL x1 (_PHI x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4145_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RP_TS x0 x2) (_RP_PL (_RP_TS x1 x2) (_RP_TS (_PHI x0 x1 x2) x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_TS x0 x2) (_RP_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x0 x1) (_RP_MORE (_RP_TS x2 x0) (_RP_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145E (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_RP_TS x2 x0) (_RP_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145F (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS x0 x1) (_RP_LESS (_RP_TS x2 x0) (_RP_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145G (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145H (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_TS x2 x0) (_RP_TS x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145J (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ145K (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_TS x2 x0) (_RP_TS x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4146_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (OR (_RP_IS x0 x1) (OR (_RP_MORE x0 x1) (_RP_LESS x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4146_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (AND (IMPLIES (_RP_IS (_RP_TS x0 x2) (_RP_TS x1 x2)) (NOT (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x2)))) (AND (IMPLIES (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x2)) (NOT (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x2)))) (IMPLIES (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x2)) (NOT (_RP_IS (_RP_TS x0 x2) (_RP_TS x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ146A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x2)) (_RP_MORE x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ146B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_TS x0 x2) (_RP_TS x1 x2)) (_RP_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ146C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x2)) (_RP_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ146D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE (_RP_TS x2 x0) (_RP_TS x2 x1)) (_RP_MORE x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ146E (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_TS x2 x0) (_RP_TS x2 x1)) (_RP_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ146F (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_LESS (_RP_TS x2 x0) (_RP_TS x2 x1)) (_RP_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4147_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4147_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_TS x1 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ147 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ147A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ148A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ148B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (_RP_MORE (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ148C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ148D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x0 x1) (IMPLIES (_RP_LESSIS x2 x3) (_RP_LESS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4149_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_IS x2 x3) (_RP_MOREIS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4149_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_IS x0 x1) (IMPLIES (_RP_MORE x2 x3) (_RP_MOREIS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4149_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_IS x0 x1) (_RP_MOREIS (_RP_TS x0 x2) (_RP_TS x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4149_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (IMPLIES (_RP_MORE x0 x1) (_RP_MOREIS (_RP_TS x0 x2) (_RP_TS x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ149 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MOREIS x0 x1) (IMPLIES (_RP_MOREIS x2 x3) (_RP_MOREIS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ149A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESSIS x0 x1) (IMPLIES (_RP_LESSIS x2 x3) (_RP_LESSIS (_RP_TS x0 x2) (_RP_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATSET (in landau3)
 (definition (LAM (x0 (O (I I))) (_SETOF _RAT (LAM (x1 (O (I I))) (_RT_LESS x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATSET_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (_SET _RAT (_RATSET x0) (_RATSET x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_SOME (LAM (x1 (O (I I))) (_RT_LESS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x1 x0) (_RT_IN x1 (_RATSET x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (NOT (_RT_LESS x0 x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (NOT (_RT_IN x0 (_RATSET x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_RATSET x0)) (_RT_LESS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_RATSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 (_RATSET x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_RATSET x0)) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_LESS x1 x2) (_RT_LESS x2 x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_RATSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (AND (_RT_LESS x1 x2) (_RT_LESS x2 x0)) (_RT_LESS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_RATSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (AND (_RT_LESS x1 x2) (_RT_LESS x2 x0)) (_RT_LESS x2 x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_RATSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (AND (_RT_LESS x1 x2) (_RT_LESS x2 x0)) (AND (_RT_IN x2 (_RATSET x0)) (_RT_MORE x2 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_RATSET x0)) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 (_RATSET x0)) (_RT_MORE x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4150_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x1 x0) (_CUTPROP (_RATSET x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ150 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_CUTPROP (_RATSET x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFRT (in landau3)
 (definition (LAM (x0 (O (I I))) (_CUTOF (_RATSET x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFRT_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (_CUT (_RPOFRT x0) (_RPOFRT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LRTRPOFRT (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x1 x0) (_LRT (_RPOFRT x0) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LRTRPOFRTE (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RPOFRT x0) x1) (_RT_LESS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III4_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MOREIS x1 x0) (NOT (_RT_LESS x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _URTRPOFRT (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MOREIS x1 x0) (_URT (_RPOFRT x0) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1RP (in landau3)
 (definition (_RPOFRT _1RT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1RP_DEC (in landau3)
 (definition (_CUT _1RP _1RP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 _1RP) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT _1RP x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RT_LESS x3 _1RT))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 _1RP) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT _1RP x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RT_LESS x1 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 _1RP) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT _1RP x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_LRT x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 _1RP) x1) (_LRT x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Y1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (_RT_TS (_OV _1RT x2) x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Y1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT x0 x4) (IMPLIES (_RT_LESS x2 x4) (_RAT (_Y1 x0 x2 x4) (_Y1 x1 x3 x5)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_RT_LESS x1 x2) (_RT_LESS (_Y1 x0 x1 x2) _1RT))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_RT_LESS x1 x2) (_LRT _1RP (_Y1 x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_RT_LESS x1 x2) (_RT_IS (_RT_TS x2 (_Y1 x0 x1 x2)) x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (IMPLIES (_RT_LESS x1 x2) (_LRT (_RP_TS x0 _1RP) x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4151_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (_LRT (_RP_TS x0 _1RP) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ151 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS (_RP_TS x0 _1RP) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ151A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS x0 (_RP_TS x0 _1RP))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ151B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS (_RP_TS _1RP x0) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ151C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS x0 (_RP_TS _1RP x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVPROP1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (AND (_URT x0 x2) (_RT_LESS x2 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVPROP2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (AND (_URT x0 x2) (AND (_RT_SOME (_INVPROP1 x0 x2)) (_RT_IS x1 (_OV _1RT x2))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVPROP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RT_SOME (_INVPROP2 x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INV (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_SETOF _RAT (_INVPROP x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INV_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (_SET _RAT (_INV x0) (_INV x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (_INVPROP1 x0 x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (_RT_SOME (_INVPROP1 x0 x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (_INVPROP2 x0 x1 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (_INVPROP x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INV1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (_RT_IN x1 (_INV x0)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (_INVPROP x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_INVPROP2 x0 x1 x5) (_URT x0 x5))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_INVPROP2 x0 x1 x5) (_RT_SOME (_INVPROP1 x0 x5)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_INVPROP2 x0 x1 x5) (_RT_IS x1 (_OV _1RT x5)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_INVPROP2 x0 x1 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_INVPROP1 x0 x5 x6) (_URT x0 x6))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_INVPROP2 x0 x1 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_INVPROP1 x0 x5 x6) (_RT_LESS x6 x5))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_INVPROP2 x0 x1 x5) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_INVPROP1 x0 x5 x6) x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_INVPROP2 x0 x1 x5) x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVAPP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_LESS x4 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) x2)))))))))) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2X0 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RT_PL x1 x1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2X0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_URT x0 x2) (_RAT (_2X0 x0 x2) (_2X0 x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (_RT_LESS x1 (_2X0 x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (_URT x0 (_2X0 x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (_RT_IN (_OV _1RT (_2X0 x0 x1)) (_INV x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_NIS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_IS (_RT_TS (_OV _1RT x2) x2) _1RT)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_IS (_RT_TS (_OV _1RT x1) x1) _1RT)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_RT_IS (_OV _1RT x2) (_OV _1RT x1)) (_RT_IS (_RT_TS (_OV _1RT x2) x1) _1RT))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_RT_IS (_OV _1RT x2) (_OV _1RT x1)) (_RT_IS x2 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_NIS (_OV _1RT x2) (_OV _1RT x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (IMPLIES (_RT_IN (_OV _1RT x1) (_INV x0)) FALSE)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (NOT (_RT_IN (_OV _1RT x1) (_INV x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_LESS x2 (_OV _1RT x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_IS (_RT_TS (_OV _1RT x3) x3) (_RT_TS x2 (_OV _1RT x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_LESS (_RT_TS x2 x3) (_RT_TS x2 (_OV _1RT x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_LESS (_RT_TS x3 x2) (_RT_TS (_OV _1RT x2) x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T28 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_LESS x3 (_OV _1RT x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T29 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_URT x0 (_OV _1RT x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T30 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_IS (_RT_TS (_OV _1RT x2) x2) _1RT))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T31 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_IS x2 (_OV _1RT (_OV _1RT x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T32 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_IS x1 (_OV _1RT x3)) (_RT_IN x2 (_INV x0)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T33 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 (_INV x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T34 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (_RT_SOME (LAM (x4 (O (I I))) (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T35 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_LESS x3 x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T36 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_URT x0 x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T37 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_IN (_OV _1RT x4) (_INV x0))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T38 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_LESS x4 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T39 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_IS (_RT_TS x2 (_OV _1RT x2)) (_RT_TS x4 (_OV _1RT x4)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T40 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_LESS (_RT_TS x4 (_OV _1RT x2)) (_RT_TS x4 (_OV _1RT x4)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T41 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_LESS (_RT_TS (_OV _1RT x2) x4) (_RT_TS (_OV _1RT x4) x4))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T42 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_LESS (_OV _1RT x2) (_OV _1RT x4))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T43 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_MORE (_OV _1RT x4) x1)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T44 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (AND (_RT_IN (_OV _1RT x4) (_INV x0)) (_RT_MORE (_OV _1RT x4) x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T45 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RT_LESS x3 x4) (_RT_LESS x4 x2)) (_RT_SOME (LAM (x5 (O (I I))) (AND (_RT_IN x5 (_INV x0)) (_RT_MORE x5 x1))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T46 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_URT x0 x3) (IMPLIES (_RT_LESS x3 x2) (IMPLIES (_RT_IS x1 (_OV _1RT x2)) (_RT_SOME (LAM (x4 (O (I I))) (AND (_RT_IN x4 (_INV x0)) (_RT_MORE x4 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T47 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_INV x0)) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 (_INV x0)) (_RT_MORE x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T48 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_CUTPROP (_INV x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T49 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (_CUTPROP (_INV x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T50 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP (_INV x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_CHI (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_CUTOF (_INV x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_CHI_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (_CUT (_4152_CHI x0) (_4152_CHI x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T51 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 (_4152_CHI x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_4152_CHI x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS x3 (_OV _1RT x4)) (_RT_IS x1 (_RT_TS x2 (_OV _1RT x4)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T52 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 (_4152_CHI x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_4152_CHI x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS x3 (_OV _1RT x4)) (_RT_LESS x2 x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T53 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 (_4152_CHI x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_4152_CHI x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS x3 (_OV _1RT x4)) (_RT_LESS x1 _1RT)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T54 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 (_4152_CHI x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_4152_CHI x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS x3 (_OV _1RT x4)) (_LRT _1RP x1)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 (_4152_CHI x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_4152_CHI x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RT_IN x3 (_INV x0)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 (_4152_CHI x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_4152_CHI x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_LRT _1RP x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_RP_TS x0 (_4152_CHI x0)) x1) (_LRT _1RP x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T55 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (_RT_LESS x1 _1RT)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T56 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (_RT_MORE _1RT x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T57 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (_RT_MORE x4 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T58 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS x2 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T59 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS (_RT_TS (_RT_MN _1RT x1) x2) (_RT_TS (_RT_MN _1RT x1) x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4152_T60 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T61 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_IS (_RT_PL (_RT_TS (_RT_MN _1RT x1) x4) (_RT_TS x1 x4)) (_RT_PL (_RT_MN x4 x3) x3)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T62 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS (_RT_PL (_RT_MN x4 x3) (_RT_TS x1 x4)) (_RT_PL (_RT_TS (_RT_MN _1RT x1) x4) (_RT_TS x1 x4))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T63 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS (_RT_PL (_RT_MN x4 x3) (_RT_TS x1 x4)) (_RT_PL (_RT_MN x4 x3) x3)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T64 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS (_RT_PL (_RT_TS x1 x4) (_RT_MN x4 x3)) (_RT_PL x3 (_RT_MN x4 x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T65 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS (_RT_TS x1 x4) x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T66 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_IS (_RT_TS (_OV _1RT x1) (_RT_TS x1 x4)) x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T67 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_LESS x4 (_OV x3 x1)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T68 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_URT x0 (_OV x3 x1)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T69 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_IS (_RT_TS (_OV x3 x1) x1) x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T70 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_IS x1 (_RT_TS x3 (_OV _1RT (_OV x3 x1)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T71 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_RT_IN (_OV _1RT (_OV x3 x1)) (_INV x0)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T72 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_LRT (_4152_CHI x0) (_OV _1RT (_OV x3 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T73 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x0 x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_URT x0 x4) (IMPLIES (_RT_IS (_RT_MN x4 x3) (_RT_TS (_RT_MN _1RT x1) x2)) (_LRT (_RP_TS x0 (_4152_CHI x0)) x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T74 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_LRT (_RP_TS x0 (_4152_CHI x0)) x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T75 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT _1RP x1) (_LRT (_RP_TS x0 (_4152_CHI x0)) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T76 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS (_RP_TS x0 (_4152_CHI x0)) _1RP)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ152 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_SOME (LAM (x1 (O (O (I I)))) (_RP_IS (_RP_TS x0 x1) _1RP)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x2 x3) (_RP_MORE (_RP_TS x1 x2) (_RP_TS x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_MORE x2 x3) (_RP_NIS (_RP_TS x1 x2) (_RP_TS x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x2 x3) (_RP_LESS (_RP_TS x1 x2) (_RP_TS x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_LESS x2 x3) (_RP_NIS (_RP_TS x1 x2) (_RP_TS x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_NIS x2 x3) (OR (_RP_MORE x2 x3) (_RP_LESS x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_NIS x2 x3) (_RP_NIS (_RP_TS x1 x2) (_RP_TS x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x3 x3) (IMPLIES (_RP_IS (_RP_TS x1 x2) x0) (IMPLIES (_RP_IS (_RP_TS x1 x3) x0) (_RP_IS x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_CHI (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (O (I I)))) (_RP_TS x2 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_CHI_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (O (I I)))) (FORALL (LAM (x5 (O (O (I I)))) (IMPLIES (_CUT x4 x5) (IMPLIES (_RP_IS (_RP_TS x2 x4) _1RP) (_CUT (_4153_CHI x0 x2 x4) (_4153_CHI x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_TS x1 x2) _1RP) (_RP_IS (_RP_TS x1 (_4153_CHI x0 x1 x2)) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_TS x1 x2) _1RP) (_RP_SOME (LAM (x3 (O (O (I I)))) (_RP_IS (_RP_TS x1 x3) x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_SOME (LAM (x2 (O (O (I I)))) (_RP_IS (_RP_TS x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4153_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_AMONE _CUT (LAM (x2 (O (O (I I)))) (_RP_IS (_RP_TS x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_ONE (LAM (x2 (O (O (I I)))) (_RP_IS (_RP_TS x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_OV (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (THAT (LAM (x2 (O (O (I I)))) (AND (_CUT x2 x2) (_RP_IS (_RP_TS x1 x2) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_OV_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_CUT (_RP_OV x0 x2) (_RP_OV x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS (_RP_TS x1 (_RP_OV x0 x1)) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS x0 (_RP_TS x1 (_RP_OV x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153E (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS (_RP_TS (_RP_OV x0 x1) x1) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153F (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (_RP_IS x0 (_RP_TS (_RP_OV x0 x1) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ153G (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_RP_TS x1 x2) x0) (_RP_IS x2 (_RP_OV x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATRP (in landau3)
 (definition (_IMAGE _RAT _CUT _RPOFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATRPI (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RATRP (_RPOFRT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFNT (in landau3)
 (definition (LAM (x0 I) (_RPOFRT (_RTOFN x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFNT_DEC (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_CUT (_RPOFNT x0) (_RPOFNT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATRP (in landau3)
 (definition (_IMAGE = _CUT _RPOFNT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATRPI (in landau3)
 (definition (FORALL (LAM (x0 I) (_NATRP (_RPOFNT x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 I) (IMPLIES (_RP_IS x0 (_RPOFNT x1)) (_RATRP x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEMMAIII5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (_RATRP x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5154_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_LRT (_RPOFRT x0) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5154_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_URT (_RPOFRT x1) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5154_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (AND (_LRT (_RPOFRT x0) x1) (_URT (_RPOFRT x1) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ154A (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RP_MORE (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ154B (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IS x0 x1) (_RP_IS (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ154C (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (_RP_LESS (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5154_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5154_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (AND (IMPLIES (_RP_IS (_RPOFRT x0) (_RPOFRT x1)) (NOT (_RP_MORE (_RPOFRT x0) (_RPOFRT x1)))) (AND (IMPLIES (_RP_MORE (_RPOFRT x0) (_RPOFRT x1)) (NOT (_RP_LESS (_RPOFRT x0) (_RPOFRT x1)))) (IMPLIES (_RP_LESS (_RPOFRT x0) (_RPOFRT x1)) (NOT (_RP_IS (_RPOFRT x0) (_RPOFRT x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ154D (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_MORE (_RPOFRT x0) (_RPOFRT x1)) (_RT_MORE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ154E (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_IS (_RPOFRT x0) (_RPOFRT x1)) (_RT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ154F (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RPOFRT x0) (_RPOFRT x1)) (_RT_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T2 (in landau3)
 (definition (_INJECTIVE _RAT _CUT _RPOFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTERP (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IS x0 x1) (_RP_IS (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTIRP (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_IS (_RPOFRT x0) (_RPOFRT x1)) (_RT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFRP (in landau3)
 (definition (_SOFT _RAT _CUT _RPOFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFRP_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RATRP x0) (_RAT (_RTOFRP x0) (_RTOFRP x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPERT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RATRP x1) (IMPLIES (_RP_IS x0 x1) (_RT_IS (_RTOFRP x0) (_RTOFRP x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPIRT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RATRP x1) (IMPLIES (_RT_IS (_RTOFRP x0) (_RTOFRP x1)) (_RP_IS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTRP1 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS x0 (_RTOFRP (_RPOFRT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTRP2 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS (_RTOFRP (_RPOFRT x0)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPRT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RP_IS x0 (_RPOFRT (_RTOFRP x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPRT2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RP_IS (_RPOFRT (_RTOFRP x0)) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTERP (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_RP_IS (_RPOFNT x0) (_RPOFNT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTIRP (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_RP_IS (_RPOFNT x0) (_RPOFNT x1)) (= x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T3 (in landau3)
 (definition (_INJECTIVE = _CUT _RPOFNT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFRP (in landau3)
 (definition (_SOFT = _CUT _RPOFNT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFRP_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (= (_NTOFRP x0) (_NTOFRP x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPENT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_IS x0 x1) (= (_NTOFRP x0) (_NTOFRP x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPINT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (= (_NTOFRP x0) (_NTOFRP x1)) (_RP_IS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTRP1 (in landau3)
 (definition (FORALL (LAM (x0 I) (= x0 (_NTOFRP (_RPOFNT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTRP2 (in landau3)
 (definition (FORALL (LAM (x0 I) (= (_NTOFRP (_RPOFNT x0)) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPNT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (_RP_IS x0 (_RPOFNT (_NTOFRP x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPNT2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (_RP_IS (_RPOFNT (_NTOFRP x0)) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_LESS x3 x0)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_LESS x4 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_LESS (_RT_PL x3 x4) (_RT_PL x0 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_RT_LESS x2 (_RT_PL x0 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_PL x3 x4)) (_LRT (_RPOFRT (_RT_PL x0 x1)) x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_PL (_RPOFRT x0) (_RPOFRT x1)) x2) (_LRT (_RPOFRT (_RT_PL x0 x1)) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_PL x0 x1)) x2) (_RT_LESS x2 (_RT_PL x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _U01 (in landau3)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (_OV x2 (_RT_PL x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _U01_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT (_RPOFRT (_RT_PL x0 x2)) x4) (_RAT (_U01 x0 x2 x4) (_U01 x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_PL x0 x1)) x2) (_RT_LESS (_RT_TS (_U01 x0 x1 x2) (_RT_PL x0 x1)) (_RT_TS _1RT (_RT_PL x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_PL x0 x1)) x2) (_RT_LESS (_U01 x0 x1 x2) _1RT))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_PL x0 x1)) x2) (_RT_IS x2 (_RT_PL (_RT_TS x0 (_U01 x0 x1 x2)) (_RT_TS x1 (_U01 x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x1 _1RT) (_RT_LESS (_RT_TS x0 x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x1 _1RT) (_LRT (_RPOFRT x0) (_RT_TS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_PL x0 x1)) x2) (_LRT (_RP_PL (_RPOFRT x0) (_RPOFRT x1)) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ155A (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RP_IS (_RPOFRT (_RT_PL x0 x1)) (_RP_PL (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_IS x0 (_RT_PL (_RT_MN x0 x1) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RP_IS (_RPOFRT x0) (_RP_PL (_RPOFRT (_RT_MN x0 x1)) (_RPOFRT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RP_IS (_RP_PL (_RPOFRT x1) (_RPOFRT (_RT_MN x0 x1))) (_RPOFRT x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ155B (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RP_IS (_RPOFRT (_RT_MN x0 x1)) (_RP_MN (_RPOFRT x0) (_RPOFRT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_LESS x3 x0)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_LESS x4 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_LESS (_RT_TS x3 x4) (_RT_TS x0 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_RT_LESS x2 (_RT_TS x0 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RPOFRT x0) x3) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_LRT (_RPOFRT x1) x4) (IMPLIES (_RT_IS x2 (_RT_TS x3 x4)) (_LRT (_RPOFRT (_RT_TS x0 x1)) x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2) (_LRT (_RPOFRT (_RT_TS x0 x1)) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (_RT_LESS x2 (_RT_TS x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_RT_LESS x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_RT_LESS x3 (_RT_TS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_RT_LESS (_RT_TS (_OV x2 x3) x3) (_RT_TS _1RT x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_RT_LESS (_OV x2 x3) _1RT))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T28 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_RT_LESS (_RT_TS (_OV x3 x1) x1) (_RT_TS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T29 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_RT_LESS (_OV x3 x1) x0))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T30 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_RT_IS x2 (_RT_TS (_OV x3 x1) (_RT_TS x1 (_OV x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T31 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (AND (_RT_LESS x2 x3) (_RT_LESS x3 (_RT_TS x0 x1))) (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T32 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT (_RT_TS x0 x1)) x2) (_LRT (_RP_TS (_RPOFRT x0) (_RPOFRT x1)) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ155C (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RP_IS (_RPOFRT (_RT_TS x0 x1)) (_RP_TS (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T33 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS x0 (_RT_TS (_OV x0 x1) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T34 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RP_IS (_RPOFRT x0) (_RP_TS (_RPOFRT (_OV x0 x1)) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5155_T35 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RP_IS (_RP_TS (_RPOFRT x1) (_RPOFRT (_OV x0 x1))) (_RPOFRT x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ155D (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RP_IS (_RPOFRT (_OV x0 x1)) (_RP_OV (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ155E (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_RP_IS (_RPOFNT (_PL x0 x1)) (_RP_PL (_RPOFNT x0) (_RPOFNT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ155F (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_RP_IS (_RPOFNT (_TIMES x0 x1)) (_RP_TS (_RPOFNT x0) (_RPOFNT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_NATT (in landau3)
 (definition (_OT _CUT _NATRP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_NATT_DEC (in landau3)
 (definition (PER _NT_NATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTTOFRP (in landau3)
 (definition (_OUT _CUT _NATRP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTTOFRP_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (_NT_NATT (_NTTOFRP x0) (_NTTOFRP x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_IS (in landau3)
 (definition _NT_NATT)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_NIS (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (NOT (_RP_NT_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_ALL (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_SOME (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (EXISTS (LAM (x1 (O (O (I I)))) (AND (_NT_NATT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_ONE (in landau3)
 (definition (_E_ONE _NT_NATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_IN (in landau3)
 (definition (_ESTI _NT_NATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFNTT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) x0))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFNTT_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x1) (_CUT (_RPOFNTT x0) (_RPOFNTT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_NATRPI (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_NATRP (_RPOFNTT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPENTT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_IS x0 x1) (_RP_NT_IS (_NTTOFRP x0) (_NTTOFRP x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPINTT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_NT_IS (_NTTOFRP x0) (_NTTOFRP x1)) (_RP_IS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTTERP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (IMPLIES (_RP_NT_IS x0 x1) (_RP_IS (_RPOFNTT x0) (_RPOFNTT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTTIRP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (IMPLIES (_RP_IS (_RPOFNTT x0) (_RPOFNTT x1)) (_RP_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPNTT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (_RP_IS x0 (_RPOFNTT (_NTTOFRP x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTTRP1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_RP_NT_IS x0 (_NTTOFRP (_RPOFNTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTTOFNT (in landau3)
 (definition (LAM (x0 I) (_NTTOFRP (_RPOFNT x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTTOFNT_DEC (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_NT_NATT (_NTTOFNT x0) (_NTTOFNT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTENTT (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_RP_NT_IS (_NTTOFNT x0) (_NTTOFNT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTINTT (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_RP_NT_IS (_NTTOFNT x0) (_NTTOFNT x1)) (= x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFNTT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_NTOFRP (_RPOFNTT x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFNTT_DEC (in landau3)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTTENT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (IMPLIES (_RP_NT_IS x0 x1) (= (_NTOFNTT x0) (_NTOFNTT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTTINT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (IMPLIES (= (_NTOFNTT x0) (_NTOFNTT x1)) (_RP_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T5 (in landau3)
 (definition (FORALL (LAM (x0 I) (_RP_IS (_RPOFNT x0) (_RPOFNTT (_NTTOFNT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T6 (in landau3)
 (definition (FORALL (LAM (x0 I) (= (_NTOFRP (_RPOFNT x0)) (_NTOFNTT (_NTTOFNT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTNTT1 (in landau3)
 (definition (FORALL (LAM (x0 I) (= x0 (_NTOFNTT (_NTTOFNT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_RP_IS (_RPOFNTT x0) (_RPOFNT (_NTOFNTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_RP_NT_IS (_NTTOFRP (_RPOFNTT x0)) (_NTTOFNT (_NTOFNTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTTNT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_RP_NT_IS x0 (_NTTOFNT (_NTOFNTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTNTT2 (in landau3)
 (definition (FORALL (LAM (x0 I) (= (_NTOFNTT (_NTTOFNT x0)) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTTNT2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_RP_NT_IS (_NTTOFNT (_NTOFNTT x0)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_1T (in landau3)
 (definition (_NTTOFNT _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_1T_DEC (in landau3)
 (definition (_NT_NATT _RP_NT_1T _RP_NT_1T))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SUCT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_NTTOFNT (_SUC (_NTOFNTT x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SUCT_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x1) (_NT_NATT (_NT_SUCT x0) (_NT_SUCT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5156_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (IMPLIES (_RP_NT_IS (_NT_SUCT x0) _RP_NT_1T) (= (_SUC (_NTOFNTT x0)) _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ156A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_RP_NT_NIS (_NT_SUCT x0) _RP_NT_1T)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5156_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (IMPLIES (_RP_NT_IS (_NT_SUCT x0) (_NT_SUCT x1)) (= (_SUC (_NTOFNTT x0)) (_SUC (_NTOFNTT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ156B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (IMPLIES (_RP_NT_IS (_NT_SUCT x0) (_NT_SUCT x1)) (_RP_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_COND1 (in landau3)
 (definition (_RP_NT_IN _RP_NT_1T))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RP_NT_COND2 (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (_RP_NT_ALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RP_NT_IN x1 x0) (_RP_NT_IN (_NT_SUCT x1) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5156_PROP1 (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (LAM (x1 I) (_RP_NT_IN (_NTTOFNT x1) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5156_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (O (I I))))) (IMPLIES (_SET _NT_NATT x0 x0) (IMPLIES (_RP_NT_COND1 x0) (IMPLIES (_RP_NT_COND2 x0) (FORALL (LAM (x1 I) (IMPLIES (_5156_PROP1 x0 x1) (_RP_NT_IN (_NT_SUCT (_NTTOFNT x1)) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5156_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (O (I I))))) (IMPLIES (_SET _NT_NATT x0 x0) (IMPLIES (_RP_NT_COND1 x0) (IMPLIES (_RP_NT_COND2 x0) (FORALL (LAM (x1 I) (IMPLIES (_5156_PROP1 x0 x1) (_5156_PROP1 x0 (_SUC x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5156_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (O (I I))))) (IMPLIES (_SET _NT_NATT x0 x0) (IMPLIES (_RP_NT_COND1 x0) (IMPLIES (_RP_NT_COND2 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (_RP_NT_IN (_NTTOFNT (_NTOFNTT x1)) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ156C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (O (I I))))) (IMPLIES (_SET _NT_NATT x0 x0) (IMPLIES (_RP_NT_COND1 x0) (IMPLIES (_RP_NT_COND2 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (_RP_NT_IN x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_AX3T (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (_RP_NT_NIS (_NT_SUCT x0) _RP_NT_1T)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_AX4T (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_NATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (IMPLIES (_RP_NT_IS (_NT_SUCT x0) (_NT_SUCT x1)) (_RP_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_AX5T (in landau3)
 (definition (FORALL (LAM (x0 (O (O (O (I I))))) (IMPLIES (_SET _NT_NATT x0 x0) (IMPLIES (_RP_NT_COND1 x0) (IMPLIES (_RP_NT_COND2 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_NT_NATT x1 x1) (_RP_NT_IN x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATT (in landau3)
 (definition (_OT _CUT _RATRP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATT_DEC (in landau3)
 (definition (PER _RATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTTOFRP (in landau3)
 (definition (_OUT _CUT _RATRP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTTOFRP_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RATRP x0) (_RATT (_RTTOFRP x0) (_RTTOFRP x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTT_IS (in landau3)
 (definition _RATT)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTT_NIS (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (NOT (_RTT_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTT_ALL (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RATT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTT_SOME (in landau3)
 (definition (LAM (x0 (O (O (O (I I))))) (EXISTS (LAM (x1 (O (O (I I)))) (AND (_RATT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTT_ONE (in landau3)
 (definition (_E_ONE _RATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFRTT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) x0))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RPOFRTT_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RATT x0 x1) (_CUT (_RPOFRTT x0) (_RPOFRTT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTT_RATRPI (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (_RATRP (_RPOFRTT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPERTT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RATRP x1) (IMPLIES (_RP_IS x0 x1) (_RTT_IS (_RTTOFRP x0) (_RTTOFRP x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPIRTT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RATRP x1) (IMPLIES (_RTT_IS (_RTTOFRP x0) (_RTTOFRP x1)) (_RP_IS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTTERP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RATT x1 x1) (IMPLIES (_RTT_IS x0 x1) (_RP_IS (_RPOFRTT x0) (_RPOFRTT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTTIRP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RATT x1 x1) (IMPLIES (_RP_IS (_RPOFRTT x0) (_RPOFRTT x1)) (_RTT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRPRTT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RP_IS x0 (_RPOFRTT (_RTTOFRP x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTTRP1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (_RTT_IS x0 (_RTTOFRP (_RPOFRTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTTOFRT (in landau3)
 (definition (LAM (x0 (O (I I))) (_RTTOFRP (_RPOFRT x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTTOFRT_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (_RATT (_RTTOFRT x0) (_RTTOFRT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTERTT (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IS x0 x1) (_RTT_IS (_RTTOFRT x0) (_RTTOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTIRTT (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RTT_IS (_RTTOFRT x0) (_RTTOFRT x1)) (_RT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFRTT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_RTOFRP (_RPOFRTT x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFRTT_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RATT x0 x1) (_RAT (_RTOFRTT x0) (_RTOFRTT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTTERT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RATT x1 x1) (IMPLIES (_RTT_IS x0 x1) (_RT_IS (_RTOFRTT x0) (_RTOFRTT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTTIRT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_RATT x1 x1) (IMPLIES (_RT_IS (_RTOFRTT x0) (_RTOFRTT x1)) (_RTT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RP_IS (_RPOFRT x0) (_RPOFRTT (_RTTOFRT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS (_RTOFRP (_RPOFRT x0)) (_RTOFRTT (_RTTOFRT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTRTT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS x0 (_RTOFRTT (_RTTOFRT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (_RP_IS (_RPOFRTT x0) (_RPOFRT (_RTOFRTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III5_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (_RTT_IS (_RTTOFRP (_RPOFRTT x0)) (_RTTOFRT (_RTOFRTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTTRT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_RATT x0 x0) (_RTT_IS x0 (_RTTOFRT (_RTOFRTT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EXAMPLE2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS (_RP_TS x0 (_RP_OV _1RP x0)) _1RP)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X01 (in landau3)
 (definition _RTOFRP)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X01_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RATRP x0) (_RAT (_X01 x0) (_X01 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _S1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_SETOF _RAT (_URT x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _S1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (_SET _RAT (_S1 x0) (_S1 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_S1 x0)) (_URT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_S1 x0)) (IMPLIES (_RT_MORE (_X01 x0) x1) (_LRT (_RPOFRT (_X01 x0)) x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_S1 x0)) (IMPLIES (_RT_MORE (_X01 x0) x1) (_LRT x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_S1 x0)) (NOT (_RT_MORE (_X01 x0) x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_S1 x0)) (_RT_LESSIS (_X01 x0) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RT_LB (_S1 x0) (_X01 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_URT (_RPOFRT (_X01 x0)) (_X01 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_URT x0 (_X01 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RT_IN (_X01 x0) (_S1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RT_MIN (_S1 x0) (_X01 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ157A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RT_MIN (_SETOF _RAT (_URT x0)) (_RTOFRP x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ157B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RATRP x0) (_RT_SOME (_RT_MIN (_SETOF _RAT (_URT x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (_RT_LB (_S1 x0) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (_RT_IN x1 (_S1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (_URT x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_RT_LESS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT x0 x2) (_LRT (_RPOFRT x1) x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_IN x2 (_S1 x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_RT_MOREIS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (_URT (_RPOFRT x1) x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RPOFRT x1) x2) (_LRT x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ157C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_SETOF _RAT (_URT x0)) x1) (_RP_IS x0 (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5157_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RT_SOME (_RT_MIN (_SETOF _RAT (_URT x0)))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MIN (_S1 x0) x1) (_RATRP x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ157D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RT_SOME (_RT_MIN (_SETOF _RAT (_URT x0)))) (_RATRP x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) _RPOFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_CUT (_XR x0 x2) (_XR x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (_URT (_XR x0 x1) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (AND (_URT (_XR x0 x1) x1) (_LRT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ158A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT x0 x1) (_RP_LESS (_RPOFRT x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_S1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_SETOF _RAT (_URT x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_S1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_URT x0 x2) (_SET _RAT (_5158_S1 x0 x2) (_5158_S1 x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (_RT_MIN (_5158_S1 x0 x1) x1) (_RP_IS (_XR x0 x1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (_RT_MIN (_5158_S1 x0 x1) x1) (_RP_MOREIS (_XR x0 x1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (_RT_IN x1 (_5158_S1 x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (NOT (_RT_LB (_5158_S1 x0 x1) x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (_RT_SOME (LAM (x2 (O (I I))) (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))) (_RT_IN x2 (_5158_S1 x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))) (_URT x0 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))) (NOT (_RT_LESSIS x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))) (_RT_LESS x2 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))) (_LRT (_XR x0 x1) x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))) (AND (_LRT (_XR x0 x1) x2) (_URT x0 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (NOT (IMPLIES (_RT_IN x2 (_5158_S1 x0 x1)) (_RT_LESSIS x1 x2))) (_RP_MORE (_XR x0 x1) x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (_RP_MORE (_XR x0 x1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (IMPLIES (NOT (_RT_MIN (_5158_S1 x0 x1) x1)) (_RP_MOREIS (_XR x0 x1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ158B (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT x0 x1) (_RP_MOREIS (_RPOFRT x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RPOFRT x1) x0) (NOT (_RP_MOREIS (_XR x0 x1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RPOFRT x1) x0) (NOT (_URT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ158C (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RPOFRT x1) x0) (_LRT x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5158_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_MOREIS (_RPOFRT x1) x0) (NOT (_RP_LESS (_XR x0 x1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ158D (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_MOREIS (_RPOFRT x1) x0) (_URT x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_XR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) _RPOFRT)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_XR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_RP_LESS x0 x2) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (_CUT (_5159_XR x0 x2 x4) (_5159_XR x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) _RPOFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_RP_LESS x0 x2) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_URT x0 x4) (IMPLIES (_LRT x2 x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (_CUT (_ZR x0 x2 x4 x6) (_ZR x1 x3 x5 x7))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (IMPLIES (_RT_LESS x2 x3) (_RP_LESS x0 (_ZR x0 x1 x2 x3))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (IMPLIES (_RT_LESS x2 x3) (AND (_RP_LESS x0 (_ZR x0 x1 x2 x3)) (_RP_LESS (_ZR x0 x1 x2 x3) x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT x1 x3) (IMPLIES (_RT_LESS x2 x3) (_RT_SOME (LAM (x4 (O (I I))) (AND (_RP_LESS x0 (_RPOFRT x4)) (_RP_LESS (_RPOFRT x4) x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT x0 x2) (IMPLIES (_LRT x1 x2) (_RT_SOME (LAM (x3 (O (I I))) (AND (_RP_LESS x0 (_RPOFRT x3)) (_RP_LESS (_RPOFRT x3) x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ159 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RP_LESS x0 (_RPOFRT x2)) (_RP_LESS (_RPOFRT x2) x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (AND (_RP_LESS x0 (_5159_XR x0 x1 x2)) (_RP_LESS (_5159_XR x0 x1 x2) x1)) (AND (_RATRP (_5159_XR x0 x1 x2)) (AND (_RP_LESS x0 (_5159_XR x0 x1 x2)) (_RP_LESS (_5159_XR x0 x1 x2) x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (AND (_RP_LESS x0 (_5159_XR x0 x1 x2)) (_RP_LESS (_5159_XR x0 x1 x2) x1)) (_RP_SOME (LAM (x3 (O (O (I I)))) (AND (_RATRP x3) (AND (_RP_LESS x0 x3) (_RP_LESS x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ159A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (_RP_SOME (LAM (x2 (O (O (I I)))) (AND (_RATRP x2) (AND (_RP_LESS x0 x2) (_RP_LESS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 O) _RPOFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_RP_LESS x0 x2) (FORALL (LAM (x4 O) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RP_LESS x0 (_RPOFRT x5)) (IMPLIES (_RP_LESS (_RPOFRT x5) x2) x4))))) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (_CUT (_YR x0 x2 x4 x6) (_YR x1 x3 x4 x7)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_RPOFRT x3)) (IMPLIES (_RP_LESS (_RPOFRT x3) x1) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RP_LESS x0 (_YR x0 x1 x2 x4)) (_RP_LESS (_YR x0 x1 x2 x4) x1)) (_RP_LESS x0 (_YR x0 x1 x2 x4)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_RPOFRT x3)) (IMPLIES (_RP_LESS (_RPOFRT x3) x1) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RP_LESS x0 (_YR x0 x1 x2 x4)) (_RP_LESS (_YR x0 x1 x2 x4) x1)) (_RP_LESS (_YR x0 x1 x2 x4) x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5159_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_RPOFRT x3)) (IMPLIES (_RP_LESS (_RPOFRT x3) x1) x2))))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (AND (_RP_LESS x0 (_YR x0 x1 x2 x4)) (_RP_LESS (_YR x0 x1 x2 x4) x1)) x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ159APP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_LESS x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_RPOFRT x3)) (IMPLIES (_RP_LESS (_RPOFRT x3) x1) x2))))) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_ZR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) _RPOFRT)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_ZR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (_CUT (_5160_ZR x0 x2 x4) (_5160_ZR x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NM (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_RP_MN (_5160_ZR x0 x1 x2) (_RP_TS x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NM_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (_CUT (_NM x0 x2 x4) (_NM x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DN (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_RP_PL (_RP_PL x0 x1) _1RP)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DN_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (_CUT (_DN x0 x2 x4) (_DN x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_FR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_RP_OV (_NM x0 x1 x2) (_DN x0 x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_FR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (_CUT (_5160_FR x0 x2 x4) (_5160_FR x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZETA (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_ITE (_RP_LESS (_5160_FR x0 x1 x2) _1RP) _CUT (_5160_FR x0 x1 x2) _1RP)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZETA_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (_CUT (_ZETA x0 x2 x4) (_ZETA x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (IMPLIES (_RP_LESS (_5160_FR x0 x1 x2) _1RP) (_RP_IS (_ZETA x0 x1 x2) (_5160_FR x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (IMPLIES (_RP_LESS (_5160_FR x0 x1 x2) _1RP) (_RP_LESSIS (_ZETA x0 x1 x2) (_5160_FR x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (IMPLIES (_RP_LESS (_5160_FR x0 x1 x2) _1RP) (_RP_LESSIS (_ZETA x0 x1 x2) _1RP)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (IMPLIES (NOT (_RP_LESS (_5160_FR x0 x1 x2) _1RP)) (_RP_IS (_ZETA x0 x1 x2) _1RP)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (IMPLIES (NOT (_RP_LESS (_5160_FR x0 x1 x2) _1RP)) (_RP_LESSIS (_ZETA x0 x1 x2) _1RP)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (IMPLIES (NOT (_RP_LESS (_5160_FR x0 x1 x2) _1RP)) (_RP_LESSIS (_ZETA x0 x1 x2) (_5160_FR x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (_RP_LESSIS (_ZETA x0 x1 x2) _1RP))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (_RP_LESSIS (_ZETA x0 x1 x2) (_5160_FR x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZR1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) _RPOFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZR1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (_CUT (_ZR1 x0 x2 x4 x6) (_ZR1 x1 x3 x5 x7))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZR2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) _RPOFRT)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ZR2_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x2 x4 x6)) (IMPLIES (_RP_LESS (_ZR1 x0 x2 x4 x6) (_RP_PL x0 (_ZETA x0 x2 x4))) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (_CUT (_ZR2 x0 x2 x4 x6 x8) (_ZR2 x1 x3 x5 x7 x9)))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESS (_RP_TS (_ZR1 x0 x1 x2 x3) (_ZR2 x0 x1 x2 x3 x4)) (_RP_PL (_RP_TS (_RP_PL x0 (_ZETA x0 x1 x2)) x1) (_RP_TS (_RP_PL x0 (_ZETA x0 x1 x2)) (_ZETA x0 x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESSIS (_RP_TS (_RP_PL x0 (_ZETA x0 x1 x2)) x1) (_RP_PL (_RP_TS x0 x1) (_RP_TS x1 (_ZETA x0 x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESSIS (_RP_TS (_RP_PL x0 (_ZETA x0 x1 x2)) (_ZETA x0 x1 x2)) (_RP_TS (_RP_PL x0 _1RP) (_ZETA x0 x1 x2))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESSIS (_RP_PL (_RP_TS (_RP_PL x0 (_ZETA x0 x1 x2)) x1) (_RP_TS (_RP_PL x0 (_ZETA x0 x1 x2)) (_ZETA x0 x1 x2))) (_RP_PL (_RP_PL (_RP_TS x0 x1) (_RP_TS x1 (_ZETA x0 x1 x2))) (_RP_TS (_RP_PL x0 _1RP) (_ZETA x0 x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESS (_RP_TS (_ZR1 x0 x1 x2 x3) (_ZR2 x0 x1 x2 x3 x4)) (_RP_PL (_RP_PL (_RP_TS x0 x1) (_RP_TS x1 (_ZETA x0 x1 x2))) (_RP_TS (_RP_PL x0 _1RP) (_ZETA x0 x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_IS (_RP_PL x1 (_RP_PL x0 _1RP)) (_DN x0 x1 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_IS (_RP_PL (_RP_TS x1 (_ZETA x0 x1 x2)) (_RP_TS (_RP_PL x0 _1RP) (_ZETA x0 x1 x2))) (_RP_TS (_DN x0 x1 x2) (_ZETA x0 x1 x2))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_IS (_RP_PL (_RP_PL (_RP_TS x0 x1) (_RP_TS x1 (_ZETA x0 x1 x2))) (_RP_TS (_RP_PL x0 _1RP) (_ZETA x0 x1 x2))) (_RP_PL (_RP_TS x0 x1) (_RP_TS (_DN x0 x1 x2) (_ZETA x0 x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESS (_RP_TS (_ZR1 x0 x1 x2 x3) (_ZR2 x0 x1 x2 x3 x4)) (_RP_PL (_RP_TS x0 x1) (_RP_TS (_DN x0 x1 x2) (_ZETA x0 x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESSIS (_RP_TS (_DN x0 x1 x2) (_ZETA x0 x1 x2)) (_NM x0 x1 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESSIS (_RP_PL (_RP_TS x0 x1) (_RP_TS (_DN x0 x1 x2) (_ZETA x0 x1 x2))) (_RP_PL (_RP_TS x0 x1) (_NM x0 x1 x2))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESS (_RP_TS (_ZR1 x0 x1 x2 x3) (_ZR2 x0 x1 x2 x3 x4)) (_RP_PL (_RP_TS x0 x1) (_NM x0 x1 x2))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_LESS (_RP_TS (_ZR1 x0 x1 x2 x3) (_ZR2 x0 x1 x2 x3 x4)) (_5160_ZR x0 x1 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RT_LESS (_RT_TS x3 x4) x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X0 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (_OV x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x2 x4 x6)) (IMPLIES (_RP_LESS (_ZR1 x0 x2 x4 x6) (_RP_PL x0 (_ZETA x0 x2 x4))) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (IMPLIES (_RP_LESS x2 (_ZR2 x0 x2 x4 x6 x8)) (IMPLIES (_RP_LESS (_ZR2 x0 x2 x4 x6 x8) (_RP_PL x2 (_ZETA x0 x2 x4))) (_RAT (_X0 x0 x2 x4 x6 x8) (_X0 x1 x3 x5 x7 x9)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_XR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (_RPOFRT (_X0 x0 x1 x2 x3 x4))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_XR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x2 x4 x6)) (IMPLIES (_RP_LESS (_ZR1 x0 x2 x4 x6) (_RP_PL x0 (_ZETA x0 x2 x4))) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (IMPLIES (_RP_LESS x2 (_ZR2 x0 x2 x4 x6 x8)) (IMPLIES (_RP_LESS (_ZR2 x0 x2 x4 x6 x8) (_RP_PL x2 (_ZETA x0 x2 x4))) (_CUT (_5160_XR x0 x2 x4 x6 x8) (_5160_XR x1 x3 x5 x7 x9)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_YR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) _RPOFRT)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_YR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x2 x4 x6)) (IMPLIES (_RP_LESS (_ZR1 x0 x2 x4 x6) (_RP_PL x0 (_ZETA x0 x2 x4))) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (IMPLIES (_RP_LESS x2 (_ZR2 x0 x2 x4 x6 x8)) (IMPLIES (_RP_LESS (_ZR2 x0 x2 x4 x6 x8) (_RP_PL x2 (_ZETA x0 x2 x4))) (_CUT (_5160_YR x0 x2 x4 x6 x8) (_5160_YR x1 x3 x5 x7 x9)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RT_IS (_RT_TS (_X0 x0 x1 x2 x3 x4) x4) x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RT_MORE (_RT_TS (_X0 x0 x1 x2 x3 x4) x4) (_RT_TS x3 x4)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RT_MORE (_X0 x0 x1 x2 x3 x4) x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_MORE (_5160_XR x0 x1 x2 x3 x4) x0))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RP_MORE (_5160_YR x0 x1 x2 x3 x4) x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) _RPOFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (_CUT (_UR x0 x2 x4 x6) (_UR x1 x3 x5 x7)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _VR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) _RPOFRT)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _VR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_RAT x8 x9) (_CUT (_VR x0 x2 x4 x6 x8) (_VR x1 x3 x5 x7 x9))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_PROP1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (LAM (x4 (O (I I))) (AND (_RP_MORE (_UR x0 x1 x2 x3) x0) (AND (_RP_MORE (_VR x0 x1 x2 x3 x4) x1) (_RT_IS (_RT_TS x3 x4) x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_PROP2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (_RT_SOME (LAM (x3 (O (I I))) (_RT_SOME (_5160_PROP1 x0 x1 x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T28 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_5160_PROP1 x0 x1 x2 (_X0 x0 x1 x2 x3 x4) x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T29 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_RT_SOME (_5160_PROP1 x0 x1 x2 (_X0 x0 x1 x2 x3 x4))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T30 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_LESS x1 (_ZR2 x0 x1 x2 x3 x4)) (IMPLIES (_RP_LESS (_ZR2 x0 x1 x2 x3 x4) (_RP_PL x1 (_ZETA x0 x1 x2))) (_5160_PROP2 x0 x1 x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T31 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_LESS x0 (_ZR1 x0 x1 x2 x3)) (IMPLIES (_RP_LESS (_ZR1 x0 x1 x2 x3) (_RP_PL x0 (_ZETA x0 x1 x2))) (_5160_PROP2 x0 x1 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ160 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (_RT_SOME (LAM (x3 (O (I I))) (_RT_SOME (LAM (x4 (O (I I))) (AND (_RP_MORE (_RPOFRT x3) x0) (AND (_RP_MORE (_RPOFRT x4) x1) (_RT_IS (_RT_TS x3 x4) x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XR1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 O) _RPOFRT)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XR1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (FORALL (LAM (x6 O) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_RP_MORE (_RPOFRT x7) x0) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_RP_MORE (_RPOFRT x8) x2) (IMPLIES (_RT_IS (_RT_TS x7 x8) x4) x6))))))))) (FORALL (LAM (x9 (O (I I))) (FORALL (LAM (x10 (O (I I))) (IMPLIES (_RAT x9 x10) (_CUT (_XR1 x0 x2 x4 x6 x9) (_XR1 x1 x3 x5 x6 x10))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YR1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (I I))) (LAM (x3 O) (LAM (x4 (O (I I))) _RPOFRT))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YR1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_RPOFRT x4) (_RP_TS x0 x2)) (FORALL (LAM (x6 O) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_RP_MORE (_RPOFRT x7) x0) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_RAT x8 x8) (IMPLIES (_RP_MORE (_RPOFRT x8) x2) (IMPLIES (_RT_IS (_RT_TS x7 x8) x4) x6))))))))) (FORALL (LAM (x9 (O (I I))) (FORALL (LAM (x10 (O (I I))) (IMPLIES (_RAT x9 x10) (IMPLIES (_RT_SOME (_5160_PROP1 x0 x2 x4 x9)) (FORALL (LAM (x11 (O (I I))) (FORALL (LAM (x12 (O (I I))) (IMPLIES (_RAT x11 x12) (_CUT (_YR1 x0 x2 x4 x6 x9 x11) (_YR1 x1 x3 x5 x6 x10 x12))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T32 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_MORE (_RPOFRT x4) x0) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RP_MORE (_RPOFRT x5) x1) (IMPLIES (_RT_IS (_RT_TS x4 x5) x2) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_5160_PROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_5160_PROP1 x0 x1 x2 x6 x7) (_RP_MORE (_XR1 x0 x1 x2 x3 x6) x0)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T33 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_MORE (_RPOFRT x4) x0) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RP_MORE (_RPOFRT x5) x1) (IMPLIES (_RT_IS (_RT_TS x4 x5) x2) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_5160_PROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_5160_PROP1 x0 x1 x2 x6 x7) (_RP_MORE (_YR1 x0 x1 x2 x3 x6 x7) x1)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T34 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_MORE (_RPOFRT x4) x0) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RP_MORE (_RPOFRT x5) x1) (IMPLIES (_RT_IS (_RT_TS x4 x5) x2) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_5160_PROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_5160_PROP1 x0 x1 x2 x6 x7) (_RT_IS (_RT_TS x6 x7) x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T35 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_MORE (_RPOFRT x4) x0) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RP_MORE (_RPOFRT x5) x1) (IMPLIES (_RT_IS (_RT_TS x4 x5) x2) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_5160_PROP1 x0 x1 x2 x6)) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x7 x7) (IMPLIES (_5160_PROP1 x0 x1 x2 x6 x7) x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5160_T36 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_MORE (_RPOFRT x4) x0) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RP_MORE (_RPOFRT x5) x1) (IMPLIES (_RT_IS (_RT_TS x4 x5) x2) x3))))))))) (FORALL (LAM (x6 (O (I I))) (IMPLIES (_RAT x6 x6) (IMPLIES (_RT_SOME (_5160_PROP1 x0 x1 x2 x6)) x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ160APP (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_RPOFRT x2) (_RP_TS x0 x1)) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (O (I I))) (IMPLIES (_RAT x4 x4) (IMPLIES (_RP_MORE (_RPOFRT x4) x0) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x5 x5) (IMPLIES (_RP_MORE (_RPOFRT x5) x1) (IMPLIES (_RT_IS (_RT_TS x4 x5) x2) x3))))))))) x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_MIN (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_ITE (_RP_LESS x0 x1) _CUT x0 x1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_MIN_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_CUT (_5161_MIN x0 x2) (_5161_MIN x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_MAX (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_ITE (_RP_MORE x0 x1) _CUT x0 x1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_MAX_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (_CUT (_5161_MAX x0 x2) (_5161_MAX x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_UR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) _RPOFRT)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_UR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (_CUT (_5161_UR x0 x2 x4) (_5161_UR x1 x3 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN x0 x1) x2) (_RP_LESS (_5161_UR x0 x1 x2) (_5161_MIN x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN x0 x1) x2) (IMPLIES (_RP_LESS x0 x1) (_RP_LESS (_5161_UR x0 x1 x2) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN x0 x1) x2) (IMPLIES (_RP_LESS x0 x1) (_RP_LESS (_5161_UR x0 x1 x2) x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN x0 x1) x2) (IMPLIES (NOT (_RP_LESS x0 x1)) (_RP_LESS (_5161_UR x0 x1 x2) x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN x0 x1) x2) (IMPLIES (NOT (_RP_LESS x0 x1)) (_RP_LESS (_5161_UR x0 x1 x2) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN x0 x1) x2) (_RP_LESS (_5161_UR x0 x1 x2) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN x0 x1) x2) (_RP_LESS (_5161_UR x0 x1 x2) x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX x0 x1) x2) (_RP_MOREIS (_5161_UR x0 x1 x2) (_5161_MAX x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX x0 x1) x2) (IMPLIES (_RP_MORE x0 x1) (_RP_MOREIS (_5161_UR x0 x1 x2) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX x0 x1) x2) (IMPLIES (_RP_MORE x0 x1) (_RP_MOREIS (_5161_UR x0 x1 x2) x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX x0 x1) x2) (IMPLIES (NOT (_RP_MORE x0 x1)) (_RP_MOREIS (_5161_UR x0 x1 x2) x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX x0 x1) x2) (IMPLIES (NOT (_RP_MORE x0 x1)) (_RP_MOREIS (_5161_UR x0 x1 x2) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX x0 x1) x2) (_RP_MOREIS (_5161_UR x0 x1 x2) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX x0 x1) x2) (_RP_MOREIS (_5161_UR x0 x1 x2) x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x1 x2) (_RP_MORE (_RP_TS x1 x1) (_RP_TS x2 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQ1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (O (I I)))) (_RP_TS x1 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQ1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (O (I I)))) (FORALL (LAM (x5 (O (O (I I)))) (IMPLIES (_CUT x4 x5) (_CUT (_SQ1 x0 x2 x4) (_SQ1 x1 x3 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQ2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (LAM (x2 (O (O (I I)))) (_RP_TS x2 x2)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQ2_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (FORALL (LAM (x4 (O (O (I I)))) (FORALL (LAM (x5 (O (O (I I)))) (IMPLIES (_CUT x4 x5) (_CUT (_SQ2 x0 x2 x4) (_SQ2 x1 x3 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_MORE x1 x2) (_RP_NIS (_SQ1 x0 x1 x2) (_SQ2 x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_SQ1 x0 x1 x2) x0) (IMPLIES (_RP_IS (_SQ2 x0 x1 x2) x0) (_RP_IS (_SQ1 x0 x1 x2) (_SQ2 x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_SQ1 x0 x1 x2) x0) (IMPLIES (_RP_IS (_SQ2 x0 x1 x2) x0) (NOT (_RP_MORE x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T19 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_SQ1 x0 x1 x2) x0) (IMPLIES (_RP_IS (_SQ2 x0 x1 x2) x0) (NOT (_RP_LESS x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (FORALL (LAM (x2 (O (O (I I)))) (IMPLIES (_CUT x2 x2) (IMPLIES (_RP_IS (_SQ1 x0 x1 x2) x0) (IMPLIES (_RP_IS (_SQ2 x0 x1 x2) x0) (_RP_IS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_AMONE _CUT (LAM (x1 (O (O (I I)))) (_RP_IS (_RP_TS x1 x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQRTSET (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_SETOF _RAT (LAM (x1 (O (I I))) (_RP_LESS (_RP_TS (_RPOFRT x1) (_RPOFRT x1)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQRTSET_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (_SET _RAT (_SQRTSET x0) (_SQRTSET x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_XR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) _RPOFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_XR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_CUT (_5161_XR x0 x2) (_5161_XR x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_5161_MIN _1RP x0) x1) (_RP_LESS (_5161_XR x0 x1) _1RP)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_5161_MIN _1RP x0) x1) (_RP_LESS (_5161_XR x0 x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_5161_MIN _1RP x0) x1) (_RP_LESS (_RP_TS (_5161_XR x0 x1) _1RP) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_5161_MIN _1RP x0) x1) (_RP_LESS (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T26 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_5161_MIN _1RP x0) x1) (_RT_IN x1 (_SQRTSET x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T27 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT (_5161_MAX _1RP x0) x1) (_RP_MOREIS (_5161_XR x0 x1) _1RP)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T28 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT (_5161_MAX _1RP x0) x1) (_RP_MOREIS (_5161_XR x0 x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T29 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT (_5161_MAX _1RP x0) x1) (_RP_MOREIS (_RP_TS (_5161_XR x0 x1) _1RP) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T30 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT (_5161_MAX _1RP x0) x1) (_RP_MOREIS (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T31 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT (_5161_MAX _1RP x0) x1) (NOT (_RP_LESS (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T32 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_URT (_5161_MAX _1RP x0) x1) (NOT (_RT_IN x1 (_SQRTSET x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_YR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) _RPOFRT)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_YR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RT_IN x2 (_SQRTSET x0)) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (_CUT (_5161_YR x0 x2 x4) (_5161_YR x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T33 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (_RP_LESS (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T34 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RP_LESS (_5161_YR x0 x1 x2) (_5161_XR x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T35 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RP_LESS (_RP_TS (_5161_YR x0 x1 x2) (_5161_YR x0 x1 x2)) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T36 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x2 x1) (_RT_IN x2 (_SQRTSET x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T37 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (_RP_MORE x0 (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_NM (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RP_MN x0 (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_NM_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RT_IN x2 (_SQRTSET x0)) (_CUT (_5161_NM x0 x2) (_5161_NM x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_DN (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RP_PL (_5161_XR x0 x1) (_RP_PL (_5161_XR x0 x1) _1RP)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_DN_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RT_IN x2 (_SQRTSET x0)) (_CUT (_5161_DN x0 x2) (_5161_DN x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_FR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_RP_OV (_5161_NM x0 x1) (_5161_DN x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_FR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RT_IN x2 (_SQRTSET x0)) (_CUT (_5161_FR x0 x2) (_5161_FR x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_ZR (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) _RPOFRT)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_ZR_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RT_IN x2 (_SQRTSET x0)) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (_CUT (_5161_ZR x0 x2 x4) (_5161_ZR x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T38 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_5161_ZR x0 x1 x2) _1RP)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T39 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_5161_ZR x0 x1 x2) (_5161_FR x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T40 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RT_MORE (_RT_PL x1 x2) x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T41 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_IS (_RP_TS (_RPOFRT (_RT_PL x1 x2)) (_RPOFRT (_RT_PL x1 x2))) (_RP_PL (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_XR x0 x1)) (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_ZR x0 x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T42 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_IS (_RP_PL (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_XR x0 x1)) (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_ZR x0 x1 x2))) (_RP_TS (_RPOFRT (_RT_PL x1 x2)) (_RPOFRT (_RT_PL x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T43 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESSIS (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_XR x0 x1)) (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_RP_TS (_5161_XR x0 x1) (_5161_ZR x0 x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T44 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_ZR x0 x1 x2)) (_RP_TS (_RP_PL (_5161_XR x0 x1) _1RP) (_5161_ZR x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T45 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_RP_PL (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_XR x0 x1)) (_RP_TS (_RP_PL (_5161_XR x0 x1) (_5161_ZR x0 x1 x2)) (_5161_ZR x0 x1 x2))) (_RP_PL (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_RP_TS (_5161_XR x0 x1) (_5161_ZR x0 x1 x2))) (_RP_TS (_RP_PL (_5161_XR x0 x1) _1RP) (_5161_ZR x0 x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T46 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_IS (_RP_PL (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_RP_TS (_5161_XR x0 x1) (_5161_ZR x0 x1 x2))) (_RP_TS (_RP_PL (_5161_XR x0 x1) _1RP) (_5161_ZR x0 x1 x2))) (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_RP_TS (_5161_DN x0 x1) (_5161_ZR x0 x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T47 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_RP_TS (_RPOFRT (_RT_PL x1 x2)) (_RPOFRT (_RT_PL x1 x2))) (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_RP_TS (_5161_DN x0 x1) (_5161_ZR x0 x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T48 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_RP_TS (_5161_DN x0 x1) (_5161_ZR x0 x1 x2)) (_5161_NM x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T49 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_RP_TS (_5161_DN x0 x1) (_5161_ZR x0 x1 x2))) (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_5161_NM x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T50 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_RP_PL (_RP_TS (_5161_XR x0 x1) (_5161_XR x0 x1)) (_RP_TS (_5161_DN x0 x1) (_5161_ZR x0 x1 x2))) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T51 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RP_LESS (_RP_TS (_RPOFRT (_RT_PL x1 x2)) (_RPOFRT (_RT_PL x1 x2))) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T52 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RT_IN (_RT_PL x1 x2) (_SQRTSET x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T53 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (AND (_RT_IN (_RT_PL x1 x2) (_SQRTSET x0)) (_RT_MORE (_RT_PL x1 x2) x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T54 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_5161_MIN _1RP (_5161_FR x0 x1)) x2) (_RT_SOME (LAM (x3 (O (I I))) (AND (_RT_IN x3 (_SQRTSET x0)) (_RT_MORE x3 x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T55 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IN x1 (_SQRTSET x0)) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_IN x2 (_SQRTSET x0)) (_RT_MORE x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T56 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_5161_MIN _1RP x0) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_URT (_5161_MAX _1RP x0) x2) (_CUTPROP (_SQRTSET x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T57 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_LRT (_5161_MIN _1RP x0) x1) (_CUTPROP (_SQRTSET x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T58 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_CUTPROP (_SQRTSET x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTC (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (_CUTOF (_SQRTSET x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTC_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (_CUT (_RTC x0) (_RTC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T59 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESSIS x0 x1) (_RP_LESSIS (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T60 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MOREIS x0 x1) (_RP_MOREIS (_RPOFRT x0) (_RPOFRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T61 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (_RP_LESS x0 (_RP_TS (_RTC x0) (_RTC x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_ZR1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) _RPOFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_ZR1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_CUT (_5161_ZR1 x0 x2) (_5161_ZR1 x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T62 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (_LRT (_RP_TS (_RTC x0) (_RTC x0)) x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_XR1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) _RPOFRT)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_XR1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x2) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (_CUT (_5161_XR1 x0 x2 x4) (_5161_XR1 x1 x3 x5)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XR2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) _RPOFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XR2_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x2) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT (_RTC x0) x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (_CUT (_XR2 x0 x2 x4 x6) (_XR2 x1 x3 x5 x7)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XM (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (_ITE (_RT_MORE x2 x3) _RAT x2 x3))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XM_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x2) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT (_RTC x0) x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_LRT (_RTC x0) x6) (IMPLIES (_RT_IS x2 (_RT_TS x4 x6)) (_RAT (_XM x0 x2 x4 x6) (_XM x1 x3 x5 x7)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XRM (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (_RPOFRT (_XM x0 x1 x2 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XRM_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x2) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_LRT (_RTC x0) x4) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_LRT (_RTC x0) x6) (IMPLIES (_RT_IS x2 (_RT_TS x4 x6)) (_CUT (_XRM x0 x2 x4 x6) (_XRM x1 x3 x5 x7)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T63 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (_RT_MORE x2 x3) (_RT_IS x2 (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T64 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (_RT_MORE x2 x3) (_LRT (_RTC x0) (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T65 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (_RT_MORE x2 x3) (_RT_LESSIS x2 (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T66 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (_RT_MORE x2 x3) (_RT_LESSIS x3 (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T67 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (NOT (_RT_MORE x2 x3)) (_RT_IS x3 (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T68 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (NOT (_RT_MORE x2 x3)) (_LRT (_RTC x0) (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T69 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (NOT (_RT_MORE x2 x3)) (_RT_LESSIS x3 (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T70 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (IMPLIES (NOT (_RT_MORE x2 x3)) (_RT_LESSIS x2 (_XM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T71 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_LRT (_RTC x0) (_XM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T72 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RT_LESSIS x2 (_XM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T73 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RT_LESSIS x3 (_XM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T74 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RT_IN (_XM x0 x1 x2 x3) (_SQRTSET x0)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T75 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RP_LESSIS (_5161_XR1 x0 x1 x2) (_XRM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_T76 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RP_LESSIS (_XR2 x0 x1 x2 x3) (_XRM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T77 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RP_IS (_5161_ZR1 x0 x1) (_RP_TS (_5161_XR1 x0 x1 x2) (_XR2 x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T78 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RP_LESSIS (_5161_ZR1 x0 x1) (_RP_TS (_XRM x0 x1 x2 x3) (_XRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T79 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RP_LESS (_RP_TS (_XRM x0 x1 x2 x3) (_XRM x0 x1 x2 x3)) x0))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T80 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) (_RP_LESS (_5161_ZR1 x0 x1) x0))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T81 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) FALSE)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T82 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_LRT (_RTC x0) x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_LRT (_RTC x0) x3) (IMPLIES (_RT_IS x1 (_RT_TS x2 x3)) FALSE)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T82A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS x0 (_5161_ZR1 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR1 x0 x1) (_RP_TS (_RTC x0) (_RTC x0))) FALSE))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T83 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_MORE (_RP_TS (_RTC x0) (_RTC x0)) x0) FALSE)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_ZR2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) _RPOFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_ZR2_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_CUT (_5161_ZR2 x0 x2) (_5161_ZR2 x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T84 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (_RP_MORE (_5161_ZR2 x0 x1) (_RP_TS (_RTC x0) (_RTC x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_YR1 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) _RPOFRT)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5161_YR1_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x2) x0) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (_CUT (_5161_YR1 x0 x2 x4) (_5161_YR1 x1 x3 x5)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YR2 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) _RPOFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YR2_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x2) x0) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_5161_YR1 x0 x2 x4) (_RTC x0)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (_CUT (_YR2 x0 x2 x4 x6) (_YR2 x1 x3 x5 x7)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YM (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (_ITE (_RT_LESS x2 x3) _RAT x2 x3))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YM_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x2) x0) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_5161_YR1 x0 x2 x4) (_RTC x0)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RP_MORE (_YR2 x0 x2 x4 x6) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x4 x6) x2) (_RAT (_YM x0 x2 x4 x6) (_YM x1 x3 x5 x7)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YRM (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (LAM (x3 (O (I I))) (_RPOFRT (_YM x0 x1 x2 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _YRM_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x2)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x2) x0) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RP_MORE (_5161_YR1 x0 x2 x4) (_RTC x0)) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_RAT x6 x7) (IMPLIES (_RP_MORE (_YR2 x0 x2 x4 x6) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x4 x6) x2) (_CUT (_YRM x0 x2 x4 x6) (_YRM x1 x3 x5 x7)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T85 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (_RT_LESS x2 x3) (_RT_IS x2 (_YM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T86 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (_RT_LESS x2 x3) (_RP_IS (_5161_YR1 x0 x1 x2) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T87 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (_RT_LESS x2 x3) (_RP_MORE (_YRM x0 x1 x2 x3) (_RTC x0))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T88 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (_RT_LESS x2 x3) (_RP_MOREIS (_5161_YR1 x0 x1 x2) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T89 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (_RT_LESS x2 x3) (_RP_MOREIS (_YR2 x0 x1 x2 x3) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T90 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (NOT (_RT_LESS x2 x3)) (_RT_IS x3 (_YM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T91 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (NOT (_RT_LESS x2 x3)) (_RP_IS (_YR2 x0 x1 x2 x3) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T92 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (NOT (_RT_LESS x2 x3)) (_RP_MORE (_YRM x0 x1 x2 x3) (_RTC x0))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T93 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (NOT (_RT_LESS x2 x3)) (_RP_MOREIS (_YR2 x0 x1 x2 x3) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T94 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (IMPLIES (NOT (_RT_LESS x2 x3)) (_RP_MOREIS (_5161_YR1 x0 x1 x2) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T95 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_RP_MORE (_YRM x0 x1 x2 x3) (_RTC x0)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T96 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_RP_MOREIS (_5161_YR1 x0 x1 x2) (_YRM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T97 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_RP_MOREIS (_YR2 x0 x1 x2 x3) (_YRM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T98 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_URT (_RTC x0) (_YM x0 x1 x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T99 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (NOT (_RT_IN (_YM x0 x1 x2 x3) (_SQRTSET x0))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T100 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (NOT (_RP_LESS (_RP_TS (_YRM x0 x1 x2 x3) (_YRM x0 x1 x2 x3)) x0)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T101 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_RP_MOREIS (_RP_TS (_YRM x0 x1 x2 x3) (_YRM x0 x1 x2 x3)) x0))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T101A (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_RP_MOREIS (_RP_TS (_5161_YR1 x0 x1 x2) (_YR2 x0 x1 x2 x3)) (_RP_TS (_YRM x0 x1 x2 x3) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T102 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_RP_MOREIS (_5161_ZR2 x0 x1) (_RP_TS (_YRM x0 x1 x2 x3) (_YRM x0 x1 x2 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T103 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) (_RP_MOREIS (_5161_ZR2 x0 x1) x0))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T104 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RP_MORE (_5161_YR1 x0 x1 x2) (_RTC x0)) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RP_MORE (_YR2 x0 x1 x2 x3) (_RTC x0)) (IMPLIES (_RT_IS (_RT_TS x2 x3) x1) FALSE)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T105 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) (_5161_ZR2 x0 x1)) (IMPLIES (_RP_LESS (_5161_ZR2 x0 x1) x0) FALSE))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T106 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_RP_LESS (_RP_TS (_RTC x0) (_RTC x0)) x0) FALSE)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T107 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS (_RP_TS (_RTC x0) (_RTC x0)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T108 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_SOME (LAM (x1 (O (O (I I)))) (_RP_IS (_RP_TS x1 x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ161 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_ONE (LAM (x1 (O (O (I I)))) (_RP_IS (_RP_TS x1 x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IRRATRP (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (NOT (_RATRP x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T1 (in landau3)
 (definition (FORALL (LAM (x0 I) (= (_PL x0 x0) (_TIMES (_SUC _1) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T2 (in landau3)
 (definition (FORALL (LAM (x0 I) (_LESS x0 (_TIMES (_SUC _1) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T3 (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS (_TIMES x0 x0) (_TIMES x1 x1)) (_LESS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T4 (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES (_PL x0 x1) x0) (_PL (_TIMES x0 x0) (_TIMES x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T5 (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES (_PL x0 x1) (_PL x0 x1)) (_PL (_PL (_PL (_TIMES x0 x0) (_TIMES x0 x1)) (_TIMES x0 x1)) (_TIMES x1 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T6 (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL (_PL (_TIMES x0 x0) (_TIMES x0 x1)) (_TIMES x0 x1)) (_PL (_TIMES x0 x0) (_TIMES (_SUC _1) (_TIMES x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NUN (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES (_PL x0 x1) (_PL x0 x1)) (_PL (_PL (_TIMES x0 x0) (_TIMES (_SUC _1) (_TIMES x0 x1))) (_TIMES x1 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NUN1 (in landau3)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL (_PL (_TIMES x0 x0) (_TIMES (_SUC _1) (_TIMES x0 x1))) (_TIMES x1 x1)) (_TIMES (_PL x0 x1) (_PL x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_PROP1 (in landau3)
 (definition (LAM (x0 I) (LAM (x1 I) (_EQ (_TF (_FR x1 x0) (_FR x1 x0)) (_FR (_SUC _1) _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_PROP2 (in landau3)
 (definition (LAM (x0 I) (_N_SOME (_5162_PROP1 x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_PROP3 (in landau3)
 (definition (_N_SOME _5162_PROP2))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_Y (in landau3)
 (definition (THAT (_MIN _5162_PROP2)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_Y_DEC (in landau3)
 (definition (IMPLIES _5162_PROP3 (= _5162_Y _5162_Y)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T7 (in landau3)
 (definition (IMPLIES _5162_PROP3 (_MIN _5162_PROP2 _5162_Y)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T8 (in landau3)
 (definition (IMPLIES _5162_PROP3 (_LB _5162_PROP2 _5162_Y)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T9 (in landau3)
 (definition (IMPLIES _5162_PROP3 (_5162_PROP2 _5162_Y)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T10 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (_EQ (_FR (_SUC _1) _1) (_FR (_TIMES x0 x0) (_TIMES _5162_Y _5162_Y))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T11 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (= (_TIMES (_SUC _1) (_TIMES _5162_Y _5162_Y)) (_TIMES x0 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T12 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (_LESS (_TIMES _5162_Y _5162_Y) (_TIMES x0 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T13 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (_LESS (_TIMES x0 x0) (_TIMES (_TIMES (_SUC _1) _5162_Y) (_TIMES (_SUC _1) _5162_Y))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T14 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (_LESS _5162_Y x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T15 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (_LESS x0 (_TIMES (_SUC _1) _5162_Y)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T16 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (_LESS (_PL _5162_Y x1) (_PL _5162_Y _5162_Y))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T17 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (_LESS x1 _5162_Y)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T18 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL x1 x2) _5162_Y))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T19 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_TIMES x0 x0) (_PL (_PL (_TIMES _5162_Y _5162_Y) (_TIMES (_SUC _1) (_TIMES _5162_Y x1))) (_TIMES x1 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T20 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL (_TIMES x0 x0) (_TIMES x2 x2)) (_PL (_PL (_TIMES _5162_Y _5162_Y) (_TIMES (_SUC _1) (_TIMES _5162_Y x1))) (_PL (_TIMES x1 x1) (_TIMES x2 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T21 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_TIMES _5162_Y x1) (_PL (_TIMES x1 x1) (_TIMES x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T22 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_TIMES (_SUC _1) (_TIMES _5162_Y x1)) (_PL (_TIMES (_SUC _1) (_TIMES x1 x1)) (_TIMES (_SUC _1) (_TIMES x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T23 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL (_TIMES _5162_Y _5162_Y) (_TIMES (_SUC _1) (_TIMES _5162_Y x1))) (_PL (_PL (_TIMES _5162_Y _5162_Y) (_TIMES (_SUC _1) (_TIMES x1 x1))) (_TIMES (_SUC _1) (_TIMES x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T24 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL (_TIMES x0 x0) (_TIMES x2 x2)) (_PL (_PL (_TIMES _5162_Y _5162_Y) (_TIMES (_SUC _1) (_TIMES x1 x1))) (_PL (_TIMES (_SUC _1) (_TIMES x1 x2)) (_PL (_TIMES x1 x1) (_TIMES x2 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T25 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL (_TIMES (_SUC _1) (_TIMES x1 x2)) (_PL (_TIMES x1 x1) (_TIMES x2 x2))) (_TIMES _5162_Y _5162_Y)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T26 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL (_TIMES x0 x0) (_TIMES x2 x2)) (_PL (_PL (_TIMES _5162_Y _5162_Y) (_TIMES _5162_Y _5162_Y)) (_TIMES (_SUC _1) (_TIMES x1 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T27 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL (_TIMES _5162_Y _5162_Y) (_TIMES _5162_Y _5162_Y)) (_TIMES x0 x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T28 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_PL (_TIMES x0 x0) (_TIMES x2 x2)) (_PL (_TIMES x0 x0) (_TIMES (_SUC _1) (_TIMES x1 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T29 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (= (_TIMES x2 x2) (_TIMES (_SUC _1) (_TIMES x1 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T30 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (_EQ (_FR (_SUC _1) _1) (_FR (_TIMES x2 x2) (_TIMES x1 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T31 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (_5162_PROP1 x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T32 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (_5162_PROP2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T33 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) (_LESSIS _5162_Y x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T34 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) (FORALL (LAM (x2 I) (IMPLIES (= _5162_Y (_PL x1 x2)) FALSE)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T35 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_PL _5162_Y x1)) FALSE))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T36 (in landau3)
 (definition (IMPLIES _5162_PROP3 (FORALL (LAM (x0 I) (IMPLIES (_5162_PROP1 _5162_Y x0) FALSE)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T37 (in landau3)
 (definition (IMPLIES _5162_PROP3 FALSE))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T38 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_EQ (_TF x1 x1) (_FR (_SUC _1) _1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T39 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_EQ (_FR (_NUM x1) (_DEN x1)) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T40 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_EQ (_TF (_FR (_NUM x1) (_DEN x1)) (_FR (_NUM x1) (_DEN x1))) (_TF x1 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T41 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_5162_PROP1 (_DEN x1) (_NUM x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T42 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_5162_PROP2 (_DEN x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T43 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) _5162_PROP3)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T44 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) FALSE)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T45 (in landau3)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_RT_IS (_RT_TS x0 x0) (_RTOFN (_SUC _1))) FALSE)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _KSI (in landau3)
 (definition (THAT (LAM (x0 (O (O (I I)))) (AND (_CUT x0 x0) (_RP_IS (_RP_TS x0 x0) (_RPOFNT (_SUC _1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _KSI_DEC (in landau3)
 (definition (_CUT _KSI _KSI))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T46 (in landau3)
 (definition (_RP_IS (_RP_TS _KSI _KSI) (_RPOFNT (_SUC _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_X0 (in landau3)
 (definition (_RTOFRP _KSI))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_X0_DEC (in landau3)
 (definition (IMPLIES (_RATRP _KSI) (_RAT _5162_X0 _5162_X0)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T47 (in landau3)
 (definition (IMPLIES (_RATRP _KSI) (_RP_IS (_RPOFRT (_RT_TS _5162_X0 _5162_X0)) (_RPOFNT (_SUC _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T48 (in landau3)
 (definition (IMPLIES (_RATRP _KSI) (_RT_IS (_RT_TS _5162_X0 _5162_X0) (_RTOFN (_SUC _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5162_T49 (in landau3)
 (definition (IMPLIES (_RATRP _KSI) FALSE))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ162 (in landau3)
 (definition (_RP_SOME _IRRATRP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQRT (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (THAT (LAM (x1 (O (O (I I)))) (AND (_CUT x1 x1) (_RP_IS (_RP_TS x1 x1) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SQRT_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (_CUT (_SQRT x0) (_SQRT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THSQRT1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (_RP_IS (_RP_TS (_SQRT x0) (_SQRT x0)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THSQRT2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS (_RP_TS x1 x1) x0) (_RP_IS x1 (_SQRT x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSQRT (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_RP_IS x0 x1) (_RP_IS (_SQRT x0) (_SQRT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_X (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_NTOFRP x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_X_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_NATRP x2) (= (_IIIA_X x0 x2) (_IIIA_X x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_Y (in landau3)
 (definition (LAM (x0 (O (O (I I)))) _NTOFRP))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_Y_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_NATRP x2) (= (_IIIA_Y x0 x2) (_IIIA_Y x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T1 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS x0 (_RPOFNT (_IIIA_X x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T2 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS x1 (_RPOFNT (_IIIA_Y x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T3 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RP_PL x0 x1) (_RP_PL (_RPOFNT (_IIIA_X x0 x1)) (_RPOFNT (_IIIA_Y x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_X0 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_RTOFN (_IIIA_X x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_X0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_NATRP x2) (_RAT (_IIIA_X0 x0 x2) (_IIIA_X0 x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_Y0 (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_RTOFN (_IIIA_Y x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_Y0_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_NATRP x2) (_RAT (_IIIA_Y0 x0 x2) (_IIIA_Y0 x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T4 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_NATRT (_IIIA_X0 x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T5 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_NATRT (_IIIA_Y0 x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T6 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RP_PL (_RPOFNT (_IIIA_X x0 x1)) (_RPOFNT (_IIIA_Y x0 x1))) (_RPOFRT (_RT_PL (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T7 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_NATRT (_RT_PL (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XPY (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_NOFRT (_RT_PL (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XPY_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_NATRP x2) (= (_XPY x0 x2) (_XPY x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T8 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RT_IS (_RT_PL (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1)) (_RTOFN (_XPY x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T9 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RPOFRT (_RT_PL (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))) (_RPOFNT (_XPY x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T10 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RP_PL x0 x1) (_RPOFNT (_XPY x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATPL (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_NATRP (_RP_PL x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T11 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RP_TS x0 x1) (_RP_TS (_RPOFNT (_IIIA_X x0 x1)) (_RPOFNT (_IIIA_Y x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T12 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RP_TS (_RPOFNT (_IIIA_X x0 x1)) (_RPOFNT (_IIIA_Y x0 x1))) (_RPOFRT (_RT_TS (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T13 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_NATRT (_RT_TS (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XTY (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_NOFRT (_RT_TS (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XTY_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_NATRP x2) (= (_XTY x0 x2) (_XTY x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T14 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RT_IS (_RT_TS (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1)) (_RTOFN (_XTY x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T15 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RPOFRT (_RT_TS (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))) (_RPOFNT (_XTY x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T16 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_RP_IS (_RP_TS x0 x1) (_RPOFNT (_XTY x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATTS (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (_NATRP (_RP_TS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T17 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_RP_MORE (_RPOFNT (_IIIA_X x0 x1)) (_RPOFNT (_IIIA_Y x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T18 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_RT_MORE (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T20 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RP_MN x0 x1) (_RP_MN (_RPOFNT (_IIIA_X x0 x1)) (_RPOFNT (_IIIA_Y x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T21 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RP_MN (_RPOFNT (_IIIA_X x0 x1)) (_RPOFNT (_IIIA_Y x0 x1))) (_RPOFRT (_RT_MN (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T22 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_NATRT (_RT_MN (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XMY (in landau3)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (O (I I)))) (_NOFRT (_RT_MN (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XMY_DEC (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x0 x1) (IMPLIES (_NATRP x0) (FORALL (LAM (x2 (O (O (I I)))) (FORALL (LAM (x3 (O (O (I I)))) (IMPLIES (_CUT x2 x3) (IMPLIES (_NATRP x2) (IMPLIES (_RP_MORE x0 x2) (= (_XMY x0 x2) (_XMY x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T23 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_RT_IS (_RT_MN (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1)) (_RTOFN (_XMY x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T24 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RPOFRT (_RT_MN (_IIIA_X0 x0 x1) (_IIIA_Y0 x0 x1))) (_RPOFNT (_XMY x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIA_T25 (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_RP_IS (_RP_MN x0 x1) (_RPOFNT (_XMY x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATMN (in landau3)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_CUT x0 x0) (IMPLIES (_NATRP x0) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_CUT x1 x1) (IMPLIES (_NATRP x1) (IMPLIES (_RP_MORE x0 x1) (_NATRP (_RP_MN x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

