(th~defproblem _AX2_THM (in landau1)
 (conclusion conc _AX2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ISF)

"))

(th~defproblem _I1_T1_THM (in landau1)
 (conclusion conc _I1_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ESTII)

"))

(th~defproblem _I1_T2_THM (in landau1)
 (conclusion conc _I1_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _I1_T3_THM (in landau1)
 (conclusion conc _I1_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_I1_T2 _ESTII)

"))

(th~defproblem _I1_T4_THM (in landau1)
 (conclusion conc _I1_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_I1_T3 _I1_T1)

"))

(th~defproblem _INDUCTION_THM (in landau1)
 (conclusion conc _INDUCTION)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_I1_T4 _ESTIE)

"))

(th~defproblem _21_T1_THM (in landau1)
 (conclusion conc _21_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _SATZ1_THM (in landau1)
 (conclusion conc _SATZ1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4)
Should be provable from hyps (_21_T1 _TH3)

"))

(th~defproblem _22_T1_THM (in landau1)
 (conclusion conc _22_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _22_T2_THM (in landau1)
 (conclusion conc _22_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4)
Should be provable from hyps (_SATZ1)

"))

(th~defproblem _SATZ2_THM (in landau1)
 (conclusion conc _SATZ2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX3 _AX4 _AX5)
Should be provable from hyps (_22_T2 _22_T1 _INDUCTION)

"))

(th~defproblem _23_T1_THM (in landau1)
 (conclusion conc _23_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_REFIS _ORI1)

"))

(th~defproblem _23_T2_THM (in landau1)
 (conclusion conc _23_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_REFIS _SOMEI)

"))

(th~defproblem _23_T3_THM (in landau1)
 (conclusion conc _23_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_23_T2 _ORI2)

"))

(th~defproblem _23_T4_THM (in landau1)
 (conclusion conc _23_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_23_T3 _23_T1 _INDUCTION)

"))

(th~defproblem _SATZ3_THM (in landau1)
 (conclusion conc _SATZ3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_23_T4 _ORE2)

"))

(th~defproblem _23_T5_THM (in landau1)
 (conclusion conc _23_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4)
Should be provable from hyps (_TRIS1)

"))

(th~defproblem _23_T6_THM (in landau1)
 (conclusion conc _23_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4)
Should be provable from hyps (_23_T5)

"))

(th~defproblem _SATZ3A_THM (in landau1)
 (conclusion conc _SATZ3A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4 _AX5)
Should be provable from hyps (_SATZ3 _23_T6 _ONEI)

"))

(th~defproblem _24_T1_THM (in landau1)
 (conclusion conc _24_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _24_T2_THM (in landau1)
 (conclusion conc _24_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _24_T3_THM (in landau1)
 (conclusion conc _24_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T2 _24_T1 _TRIS2)

"))

(th~defproblem _24_T4_THM (in landau1)
 (conclusion conc _24_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_AX2)

"))

(th~defproblem _24_T5_THM (in landau1)
 (conclusion conc _24_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _24_T6_THM (in landau1)
 (conclusion conc _24_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _24_T7_THM (in landau1)
 (conclusion conc _24_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T5)

"))

(th~defproblem _24_T8_THM (in landau1)
 (conclusion conc _24_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T6)

"))

(th~defproblem _24_T9_THM (in landau1)
 (conclusion conc _24_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T8 _SYMIS _24_T4 _24_T7 _TR3IS)

"))

(th~defproblem _24_T10_THM (in landau1)
 (conclusion conc _24_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_24_T9 _24_T3 _INDUCTION)

"))

(th~defproblem _24_T11_THM (in landau1)
 (conclusion conc _24_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_24_T10 _FISI)

"))

(th~defproblem _AA_THM (in landau1)
 (conclusion conc _AA)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_24_T11)

"))

(th~defproblem _24_T12_THM (in landau1)
 (conclusion conc _24_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_REFIS)

"))

(th~defproblem _24_T13_THM (in landau1)
 (conclusion conc _24_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T12 _REFIS _ANDI)

"))

(th~defproblem _24_T14_THM (in landau1)
 (conclusion conc _24_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T13 _SOMEI)

"))

(th~defproblem _24_T15_THM (in landau1)
 (conclusion conc _24_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_REFIS)

"))

(th~defproblem _24_T16_THM (in landau1)
 (conclusion conc _24_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _24_T17_THM (in landau1)
 (conclusion conc _24_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T16 _AX2 _24_T15 _TRIS)

"))

(th~defproblem _24_T18_THM (in landau1)
 (conclusion conc _24_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _24_T19_THM (in landau1)
 (conclusion conc _24_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T18)

"))

(th~defproblem _24_T20_THM (in landau1)
 (conclusion conc _24_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T15 _24_T19 _TRIS2)

"))

(th~defproblem _24_T21_THM (in landau1)
 (conclusion conc _24_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T20 _AX2 _24_T15 _TRIS)

"))

(th~defproblem _24_T22_THM (in landau1)
 (conclusion conc _24_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T21)

"))

(th~defproblem _24_T23_THM (in landau1)
 (conclusion conc _24_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T22 _24_T17 _ANDI)

"))

(th~defproblem _24_T24_THM (in landau1)
 (conclusion conc _24_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T23 _SOMEI)

"))

(th~defproblem _24_T25_THM (in landau1)
 (conclusion conc _24_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
Should be provable from hyps (_24_T24 _SOMEAPP)

"))

(th~defproblem _BB_THM (in landau1)
 (conclusion conc _BB)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_24_T25 _24_T14 _INDUCTION)

"))

(th~defproblem _SATZ4_THM (in landau1)
 (conclusion conc _SATZ4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_BB _AA _ONEI)

"))

(th~defproblem _24_T26_THM (in landau1)
 (conclusion conc _24_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4)

"))

(th~defproblem _SATZ4A_THM (in landau1)
 (conclusion conc _SATZ4A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_24_T26 _ANDE1)

"))

(th~defproblem _24_T27_THM (in landau1)
 (conclusion conc _24_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_24_T26 _ANDE2)

"))

(th~defproblem _SATZ4B_THM (in landau1)
 (conclusion conc _SATZ4B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_24_T27)

"))

(th~defproblem _24_T28_THM (in landau1)
 (conclusion conc _24_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_24_T13 _24_T26 _24_T11)

"))

(th~defproblem _SATZ4C_THM (in landau1)
 (conclusion conc _SATZ4C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_24_T28 _FISE)

"))

(th~defproblem _24_T29_THM (in landau1)
 (conclusion conc _24_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_24_T26 _BB _24_T23 _24_T11)

"))

(th~defproblem _SATZ4D_THM (in landau1)
 (conclusion conc _SATZ4D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_24_T29 _FISE)

"))

(th~defproblem _SATZ4E_THM (in landau1)
 (conclusion conc _SATZ4E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4A _SYMIS)

"))

(th~defproblem _SATZ4F_THM (in landau1)
 (conclusion conc _SATZ4F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4B _SYMIS)

"))

(th~defproblem _SATZ4G_THM (in landau1)
 (conclusion conc _SATZ4G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4C _SYMIS)

"))

(th~defproblem _SATZ4H_THM (in landau1)
 (conclusion conc _SATZ4H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4D _SYMIS)

"))

(th~defproblem _ISPL1_THM (in landau1)
 (conclusion conc _ISPL1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _ISPL2_THM (in landau1)
 (conclusion conc _ISPL2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _ISPL12_THM (in landau1)
 (conclusion conc _ISPL12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISPL2 _ISPL1 _TRIS)

"))

(th~defproblem _25_T1_THM (in landau1)
 (conclusion conc _25_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4E _ISPL2 _SATZ4F _SATZ4A _TR3IS)

"))

(th~defproblem _25_T2_THM (in landau1)
 (conclusion conc _25_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_AX2)

"))

(th~defproblem _25_T3_THM (in landau1)
 (conclusion conc _25_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4F _ISPL2 _25_T2 _SATZ4B _TR4IS)

"))

(th~defproblem _SATZ5_THM (in landau1)
 (conclusion conc _SATZ5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_25_T3 _25_T1 _INDUCTION)

"))

(th~defproblem _ASSPL1_THM (in landau1)
 (conclusion conc _ASSPL1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ5)

"))

(th~defproblem _ASSPL2_THM (in landau1)
 (conclusion conc _ASSPL2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ5 _SYMIS)

"))

(th~defproblem _26_T1_THM (in landau1)
 (conclusion conc _26_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4A)

"))

(th~defproblem _26_T2_THM (in landau1)
 (conclusion conc _26_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4C)

"))

(th~defproblem _26_T3_THM (in landau1)
 (conclusion conc _26_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_26_T1 _26_T2 _TRIS2)

"))

(th~defproblem _26_T4_THM (in landau1)
 (conclusion conc _26_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4F _AX2 _TRIS)

"))

(th~defproblem _26_T5_THM (in landau1)
 (conclusion conc _26_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4D)

"))

(th~defproblem _26_T6_THM (in landau1)
 (conclusion conc _26_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_26_T4 _26_T5 _TRIS)

"))

(th~defproblem _SATZ6_THM (in landau1)
 (conclusion conc _SATZ6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_26_T6 _26_T3 _INDUCTION)

"))

(th~defproblem _COMPL_THM (in landau1)
 (conclusion conc _COMPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ6)

"))

(th~defproblem _26_T7_THM (in landau1)
 (conclusion conc _26_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4G _SATZ4A _TRIS)

"))

(th~defproblem _26_T8_THM (in landau1)
 (conclusion conc _26_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4H _AX2 _SATZ4B _TR3IS)

"))

(th~defproblem _ANDERS_THM (in landau1)
 (conclusion conc _ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_26_T8 _26_T7 _INDUCTION)

"))

(th~defproblem _27_T1_THM (in landau1)
 (conclusion conc _27_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX3)
Should be provable from hyps (_SYMNOTIS)

"))

(th~defproblem _27_T2_THM (in landau1)
 (conclusion conc _27_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ4A _27_T1 _NOTIS_TH4)

"))

(th~defproblem _27_T3_THM (in landau1)
 (conclusion conc _27_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4 _AX5)
Should be provable from hyps (_SATZ1)

"))

(th~defproblem _27_T4_THM (in landau1)
 (conclusion conc _27_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_SATZ4B _27_T3 _NOTIS_TH4)

"))

(th~defproblem _SATZ7_THM (in landau1)
 (conclusion conc _SATZ7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_27_T4 _27_T2 _INDUCTION)

"))

(th~defproblem _28_T1_THM (in landau1)
 (conclusion conc _28_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4)
Should be provable from hyps (_SATZ1)

"))

(th~defproblem _28_T2_THM (in landau1)
 (conclusion conc _28_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_SATZ4G _28_T1 _NOTIS_TH5)

"))

(th~defproblem _28_T3_THM (in landau1)
 (conclusion conc _28_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX4 _AX5)
Should be provable from hyps (_SATZ1)

"))

(th~defproblem _28_T4_THM (in landau1)
 (conclusion conc _28_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_SATZ4H _28_T3 _NOTIS_TH5)

"))

(th~defproblem _SATZ8_THM (in landau1)
 (conclusion conc _SATZ8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_28_T4 _28_T2 _INDUCTION)

"))

(th~defproblem _SATZ8A_THM (in landau1)
 (conclusion conc _SATZ8A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_SATZ8 _WELI _TH7)

"))

(th~defproblem _28_T5_THM (in landau1)
 (conclusion conc _28_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_TRIS1 _SATZ8A)

"))

(th~defproblem _SATZ8B_THM (in landau1)
 (conclusion conc _SATZ8B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_28_T5)

"))

(th~defproblem _29_T1_THM (in landau1)
 (conclusion conc _29_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISPL1 _COMPL _TRIS)

"))

(th~defproblem _29_T2_THM (in landau1)
 (conclusion conc _29_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T1 _SATZ7 _NOTIS_TH3)

"))

(th~defproblem _29_T3_THM (in landau1)
 (conclusion conc _29_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T2 _SOME_TH5)

"))

(th~defproblem _29_T4_THM (in landau1)
 (conclusion conc _29_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T3 _EC_TH1)

"))

(th~defproblem _29_T5_THM (in landau1)
 (conclusion conc _29_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS _29_T3)

"))

(th~defproblem _29_T6_THM (in landau1)
 (conclusion conc _29_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T5 _EC_TH2)

"))

(th~defproblem _T6A_THM (in landau1)
 (conclusion conc _T6A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMPL _ASSPL1 _ISPL1 _TR4IS)

"))

(th~defproblem _29_T7_THM (in landau1)
 (conclusion conc _29_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ7 _T6A _MP)

"))

(th~defproblem _29_T8_THM (in landau1)
 (conclusion conc _29_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T7 _SOMEAPP)

"))

(th~defproblem _29_T9_THM (in landau1)
 (conclusion conc _29_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T8 _SOMEAPP)

"))

(th~defproblem _29_T10_THM (in landau1)
 (conclusion conc _29_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T9)

"))

(th~defproblem _29_T11_THM (in landau1)
 (conclusion conc _29_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T10 _EC_TH1)

"))

(th~defproblem _A_THM (in landau1)
 (conclusion conc _A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_29_T6 _29_T11 _29_T4 _EC3_TH6)

"))

(th~defproblem _29_T12_THM (in landau1)
 (conclusion conc _29_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_OR3I1)

"))

(th~defproblem _29_T13_THM (in landau1)
 (conclusion conc _29_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4G _TRIS)

"))

(th~defproblem _29_T14_THM (in landau1)
 (conclusion conc _29_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T13 _SOMEI)

"))

(th~defproblem _29_T15_THM (in landau1)
 (conclusion conc _29_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T14 _SATZ3 _SOMEAPP)

"))

(th~defproblem _29_T16_THM (in landau1)
 (conclusion conc _29_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T15 _OR3I2)

"))

(th~defproblem _T16A_THM (in landau1)
 (conclusion conc _T16A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T16 _SATZ3 _SOMEAPP)

"))

(th~defproblem _29_T17_THM (in landau1)
 (conclusion conc _29_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_T16A _29_T12 _TH1)

"))

(th~defproblem _29_T18_THM (in landau1)
 (conclusion conc _29_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SYMIS _ISPL1 _SATZ4E _TRIS)

"))

(th~defproblem _29_T19_THM (in landau1)
 (conclusion conc _29_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T18 _SOMEI)

"))

(th~defproblem _29_T20_THM (in landau1)
 (conclusion conc _29_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T19 _OR3I3)

"))

(th~defproblem _29_T21_THM (in landau1)
 (conclusion conc _29_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4A _ISPL2 _TR3IS)

"))

(th~defproblem _29_T22_THM (in landau1)
 (conclusion conc _29_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T21 _OR3I1)

"))

(th~defproblem _29_T23_THM (in landau1)
 (conclusion conc _29_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4G _TRIS)

"))

(th~defproblem _29_T24_THM (in landau1)
 (conclusion conc _29_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4A _ISPL1 _ASSPL2 _29_T23 _ISPL2 _TR4IS)

"))

(th~defproblem _29_T25_THM (in landau1)
 (conclusion conc _29_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T24 _SOMEI)

"))

(th~defproblem _29_T26_THM (in landau1)
 (conclusion conc _29_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T25 _SATZ3 _SOMEAPP)

"))

(th~defproblem _29_T27_THM (in landau1)
 (conclusion conc _29_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T26 _OR3I2)

"))

(th~defproblem _29_T28_THM (in landau1)
 (conclusion conc _29_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T27 _29_T22 _TH1)

"))

(th~defproblem _T28A_THM (in landau1)
 (conclusion conc _T28A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T28 _SOMEAPP)

"))

(th~defproblem _29_T29_THM (in landau1)
 (conclusion conc _29_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4F _AX2 _TRIS)

"))

(th~defproblem _29_T30_THM (in landau1)
 (conclusion conc _29_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T29 _SOMEI)

"))

(th~defproblem _29_T31_THM (in landau1)
 (conclusion conc _29_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T30 _SOMEAPP)

"))

(th~defproblem _29_T32_THM (in landau1)
 (conclusion conc _29_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T31 _OR3I3)

"))

(th~defproblem _29_T33_THM (in landau1)
 (conclusion conc _29_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T32 _T28A _29_T20 _OR3APP)

"))

(th~defproblem _B_THM (in landau1)
 (conclusion conc _B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_29_T33 _29_T17 _INDUCTION)

"))

(th~defproblem _SATZ9_THM (in landau1)
 (conclusion conc _SATZ9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_A _B _OREC3I)

"))

(th~defproblem _SATZ9A_THM (in landau1)
 (conclusion conc _SATZ9A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_B)

"))

(th~defproblem _SATZ9B_THM (in landau1)
 (conclusion conc _SATZ9B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_A)

"))

(th~defproblem _SATZ10_THM (in landau1)
 (conclusion conc _SATZ10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ9)

"))

(th~defproblem _SATZ10A_THM (in landau1)
 (conclusion conc _SATZ10A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ9A)

"))

(th~defproblem _SATZ10B_THM (in landau1)
 (conclusion conc _SATZ10B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ9B)

"))

(th~defproblem _SATZ11_THM (in landau1)
 (conclusion conc _SATZ11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _SATZ12_THM (in landau1)
 (conclusion conc _SATZ12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _SATZ13_THM (in landau1)
 (conclusion conc _SATZ13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_SYMIS _SATZ11 _TH9)

"))

(th~defproblem _SATZ14_THM (in landau1)
 (conclusion conc _SATZ14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_SYMIS _SATZ12 _TH9)

"))

(th~defproblem _ISMORE1_THM (in landau1)
 (conclusion conc _ISMORE1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _ISMORE2_THM (in landau1)
 (conclusion conc _ISMORE2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _ISLESS1_THM (in landau1)
 (conclusion conc _ISLESS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _ISLESS2_THM (in landau1)
 (conclusion conc _ISLESS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _ISMOREIS1_THM (in landau1)
 (conclusion conc _ISMOREIS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _ISMOREIS2_THM (in landau1)
 (conclusion conc _ISMOREIS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _ISLESSIS1_THM (in landau1)
 (conclusion conc _ISLESSIS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _ISLESSIS2_THM (in landau1)
 (conclusion conc _ISLESSIS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _MOREISI2_THM (in landau1)
 (conclusion conc _MOREISI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _LESSISI2_THM (in landau1)
 (conclusion conc _LESSISI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _MOREISI1_THM (in landau1)
 (conclusion conc _MOREISI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _LESSISI1_THM (in landau1)
 (conclusion conc _LESSISI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _ISMORE12_THM (in landau1)
 (conclusion conc _ISMORE12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISMORE1 _ISMORE2)

"))

(th~defproblem _ISLESS12_THM (in landau1)
 (conclusion conc _ISLESS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISLESS1 _ISLESS2)

"))

(th~defproblem _ISMOREIS12_THM (in landau1)
 (conclusion conc _ISMOREIS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISMOREIS1 _ISMOREIS2)

"))

(th~defproblem _ISLESSIS12_THM (in landau1)
 (conclusion conc _ISLESSIS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISLESSIS1 _ISLESSIS2)

"))

(th~defproblem _SATZ10C_THM (in landau1)
 (conclusion conc _SATZ10C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMOR _SATZ10B _EC3_TH7)

"))

(th~defproblem _SATZ10D_THM (in landau1)
 (conclusion conc _SATZ10D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ10B _EC3_TH9)

"))

(th~defproblem _SATZ10E_THM (in landau1)
 (conclusion conc _SATZ10E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ10A _OR3_TH2)

"))

(th~defproblem _SATZ10F_THM (in landau1)
 (conclusion conc _SATZ10F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ10A _OR3_TH3 _COMOR)

"))

(th~defproblem _SATZ10G_THM (in landau1)
 (conclusion conc _SATZ10G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ10B _EC3E21 _EC3E23 _OR_TH3)

"))

(th~defproblem _SATZ10H_THM (in landau1)
 (conclusion conc _SATZ10H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ10B _EC3E31 _EC3E32 _OR_TH3)

"))

(th~defproblem _SATZ10J_THM (in landau1)
 (conclusion conc _SATZ10J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_OR_TH4 _OR_TH5 _SATZ10A _OR3E3)

"))

(th~defproblem _SATZ10K_THM (in landau1)
 (conclusion conc _SATZ10K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_OR_TH5 _OR_TH4 _SATZ10A _OR3E2)

"))

(th~defproblem _315_T1_THM (in landau1)
 (conclusion conc _315_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSPL1 _ISPL1 _TR3IS)

"))

(th~defproblem _315_T2_THM (in landau1)
 (conclusion conc _315_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_315_T1 _SOMEI)

"))

(th~defproblem _315_T3_THM (in landau1)
 (conclusion conc _315_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_315_T2 _SOMEAPP)

"))

(th~defproblem _SATZ15_THM (in landau1)
 (conclusion conc _SATZ15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_315_T3 _SOMEAPP)

"))

(th~defproblem _TRLESS_THM (in landau1)
 (conclusion conc _TRLESS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ15)

"))

(th~defproblem _TRMORE_THM (in landau1)
 (conclusion conc _TRMORE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ15)

"))

(th~defproblem _315_ANDERS_THM (in landau1)
 (conclusion conc _315_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ11 _SATZ15 _SATZ12)

"))

(th~defproblem _SATZ16A_THM (in landau1)
 (conclusion conc _SATZ16A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SYMIS _ISLESS1 _TRLESS _ORAPP)

"))

(th~defproblem _SATZ16B_THM (in landau1)
 (conclusion conc _SATZ16B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISLESS2 _TRLESS _ORAPP)

"))

(th~defproblem _SATZ16C_THM (in landau1)
 (conclusion conc _SATZ16C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ13 _SATZ16B)

"))

(th~defproblem _SATZ16D_THM (in landau1)
 (conclusion conc _SATZ16D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ13 _SATZ16A)

"))

(th~defproblem _317_T1_THM (in landau1)
 (conclusion conc _317_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_TRIS _LESSISI2)

"))

(th~defproblem _317_T2_THM (in landau1)
 (conclusion conc _317_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ16A _LESSISI1)

"))

(th~defproblem _317_T3_THM (in landau1)
 (conclusion conc _317_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_317_T1 _317_T2 _ORAPP)

"))

(th~defproblem _317_T4_THM (in landau1)
 (conclusion conc _317_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ16B _LESSISI1)

"))

(th~defproblem _SATZ17_THM (in landau1)
 (conclusion conc _SATZ17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_317_T3 _317_T4 _ORAPP)

"))

(th~defproblem _317_T5_THM (in landau1)
 (conclusion conc _317_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ16B _LESSISI1)

"))

(th~defproblem _317_T6_THM (in landau1)
 (conclusion conc _317_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_SYMIS _ISLESSIS1)

"))

(th~defproblem _317_ANDERS_THM (in landau1)
 (conclusion conc _317_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_317_T6 _317_T5 _ORAPP)

"))

(th~defproblem _TRLESSIS_THM (in landau1)
 (conclusion conc _TRLESSIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ17)

"))

(th~defproblem _TRMOREIS_THM (in landau1)
 (conclusion conc _TRMOREIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ13 _SATZ17 _SATZ14)

"))

(th~defproblem _SATZ18_THM (in landau1)
 (conclusion conc _SATZ18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_REFIS _SOMEI)

"))

(th~defproblem _SATZ18A_THM (in landau1)
 (conclusion conc _SATZ18A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_SATZ18)

"))

(th~defproblem _SATZ18B_THM (in landau1)
 (conclusion conc _SATZ18B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ18 _SATZ4A _ISMORE1)

"))

(th~defproblem _SATZ18C_THM (in landau1)
 (conclusion conc _SATZ18C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ18B)

"))

(th~defproblem _319_T1_THM (in landau1)
 (conclusion conc _319_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMPL _TRIS)

"))

(th~defproblem _319_T2_THM (in landau1)
 (conclusion conc _319_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMPL _ASSPL1 _319_T1 _ISPL1 _TR3IS)

"))

(th~defproblem _319_T3_THM (in landau1)
 (conclusion conc _319_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_319_T2 _SOMEI)

"))

(th~defproblem _SATZ19A_THM (in landau1)
 (conclusion conc _SATZ19A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_319_T3 _SOMEAPP)

"))

(th~defproblem _SATZ19B_THM (in landau1)
 (conclusion conc _SATZ19B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISPL1)

"))

(th~defproblem _SATZ19C_THM (in landau1)
 (conclusion conc _SATZ19C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ12 _SATZ19A _SATZ11)

"))

(th~defproblem _ANDERS1_THM (in landau1)
 (conclusion conc _ANDERS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19A)

"))

(th~defproblem _SATZ19D_THM (in landau1)
 (conclusion conc _SATZ19D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19A _COMPL _ISMORE12)

"))

(th~defproblem _SATZ19E_THM (in landau1)
 (conclusion conc _SATZ19E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISPL2)

"))

(th~defproblem _SATZ19F_THM (in landau1)
 (conclusion conc _SATZ19F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19C _COMPL _ISLESS12)

"))

(th~defproblem _ANDERS2_THM (in landau1)
 (conclusion conc _ANDERS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19D)

"))

(th~defproblem _SATZ19G_THM (in landau1)
 (conclusion conc _SATZ19G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19D _ISPL1 _ISMORE2)

"))

(th~defproblem _SATZ19H_THM (in landau1)
 (conclusion conc _SATZ19H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19G _COMPL _ISMORE12)

"))

(th~defproblem _SATZ19J_THM (in landau1)
 (conclusion conc _SATZ19J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19F _ISPL1 _ISLESS2)

"))

(th~defproblem _SATZ19K_THM (in landau1)
 (conclusion conc _SATZ19K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19J _COMPL _ISLESS12)

"))

(th~defproblem _319_T4_THM (in landau1)
 (conclusion conc _319_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19A _MOREISI1)

"))

(th~defproblem _319_T5_THM (in landau1)
 (conclusion conc _319_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISPL1 _MOREISI2)

"))

(th~defproblem _SATZ19L_THM (in landau1)
 (conclusion conc _SATZ19L)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_319_T5 _319_T4 _ORAPP)

"))

(th~defproblem _SATZ19M_THM (in landau1)
 (conclusion conc _SATZ19M)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19L _COMPL _ISMOREIS12)

"))

(th~defproblem _SATZ19N_THM (in landau1)
 (conclusion conc _SATZ19N)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ19L _SATZ13)

"))

(th~defproblem _SATZ19O_THM (in landau1)
 (conclusion conc _SATZ19O)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ19M _SATZ13)

"))

(th~defproblem _320_T1_THM (in landau1)
 (conclusion conc _320_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ10A)

"))

(th~defproblem _320_T2_THM (in landau1)
 (conclusion conc _320_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ10B)

"))

(th~defproblem _SATZ20A_THM (in landau1)
 (conclusion conc _SATZ20A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ19C _SATZ19A _SATZ19B _320_T2 _320_T1 _EC3_TH11)

"))

(th~defproblem _SATZ20B_THM (in landau1)
 (conclusion conc _SATZ20B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ19C _SATZ19A _SATZ19B _320_T2 _320_T1 _EC3_TH10)

"))

(th~defproblem _SATZ20C_THM (in landau1)
 (conclusion conc _SATZ20C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ19C _SATZ19A _SATZ19B _320_T2 _320_T1 _EC3_TH12)

"))

(th~defproblem _320_T3_THM (in landau1)
 (conclusion conc _320_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMPL _TR3IS)

"))

(th~defproblem _ANDERSB_THM (in landau1)
 (conclusion conc _ANDERSB)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_320_T3 _SATZ8A)

"))

(th~defproblem _ANDERSC_THM (in landau1)
 (conclusion conc _ANDERSC)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ20A)

"))

(th~defproblem _SATZ20D_THM (in landau1)
 (conclusion conc _SATZ20D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMPL _ISMORE12 _SATZ20A)

"))

(th~defproblem _SATZ20E_THM (in landau1)
 (conclusion conc _SATZ20E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMPL _TR3IS _SATZ20B)

"))

(th~defproblem _SATZ20F_THM (in landau1)
 (conclusion conc _SATZ20F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMPL _ISLESS12 _SATZ20C)

"))

(th~defproblem _321_T1_THM (in landau1)
 (conclusion conc _321_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19A)

"))

(th~defproblem _321_T2_THM (in landau1)
 (conclusion conc _321_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19A _COMPL _ISMORE12)

"))

(th~defproblem _SATZ21_THM (in landau1)
 (conclusion conc _SATZ21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_321_T2 _321_T1 _TRMORE)

"))

(th~defproblem _321_ANDERS_THM (in landau1)
 (conclusion conc _321_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19D _SATZ19A _TRMORE)

"))

(th~defproblem _SATZ21A_THM (in landau1)
 (conclusion conc _SATZ21A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ21)

"))

(th~defproblem _ANDERSA_THM (in landau1)
 (conclusion conc _ANDERSA)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ12 _SATZ21 _SATZ11)

"))

(th~defproblem _SATZ22A_THM (in landau1)
 (conclusion conc _SATZ22A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19G _SATZ21 _ORAPP)

"))

(th~defproblem _SATZ22B_THM (in landau1)
 (conclusion conc _SATZ22B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19H _SATZ21 _ORAPP)

"))

(th~defproblem _SATZ22C_THM (in landau1)
 (conclusion conc _SATZ22C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ22A)

"))

(th~defproblem _SATZ22D_THM (in landau1)
 (conclusion conc _SATZ22D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ22B)

"))

(th~defproblem _323_T1_THM (in landau1)
 (conclusion conc _323_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISPL2 _ISPL1 _TRIS _MOREISI2)

"))

(th~defproblem _323_T2_THM (in landau1)
 (conclusion conc _323_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ22A _MOREISI1)

"))

(th~defproblem _323_T3_THM (in landau1)
 (conclusion conc _323_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_323_T1 _323_T2 _ORAPP)

"))

(th~defproblem _323_T4_THM (in landau1)
 (conclusion conc _323_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ22B _MOREISI1)

"))

(th~defproblem _SATZ23_THM (in landau1)
 (conclusion conc _SATZ23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_323_T3 _323_T4 _ORAPP)

"))

(th~defproblem _323_T5_THM (in landau1)
 (conclusion conc _323_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ22B _MOREISI1)

"))

(th~defproblem _323_T6_THM (in landau1)
 (conclusion conc _323_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19M _ISPL1 _ISMOREIS2)

"))

(th~defproblem _323_ANDERS_THM (in landau1)
 (conclusion conc _323_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_323_T6 _323_T5 _ORAPP)

"))

(th~defproblem _SATZ23A_THM (in landau1)
 (conclusion conc _SATZ23A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ23 _SATZ13)

"))

(th~defproblem _324_T1_THM (in landau1)
 (conclusion conc _324_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4G _TRIS)

"))

(th~defproblem _324_T2_THM (in landau1)
 (conclusion conc _324_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ18 _324_T1 _SYMIS _ISMORE1)

"))

(th~defproblem _324_T3_THM (in landau1)
 (conclusion conc _324_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_324_T2 _SATZ3 _SOMEAPP)

"))

(th~defproblem _SATZ24_THM (in landau1)
 (conclusion conc _SATZ24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_324_T3 _OR_TH2)

"))

(th~defproblem _SATZ24A_THM (in landau1)
 (conclusion conc _SATZ24A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ24 _SATZ13)

"))

(th~defproblem _SATZ24B_THM (in landau1)
 (conclusion conc _SATZ24B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_324_T3)

"))

(th~defproblem _SATZ24C_THM (in landau1)
 (conclusion conc _SATZ24C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ24B)

"))

(th~defproblem _325_T1_THM (in landau1)
 (conclusion conc _325_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ24 _SATZ19M)

"))

(th~defproblem _325_T2_THM (in landau1)
 (conclusion conc _325_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_325_T1 _SYMIS _ISMOREIS1)

"))

(th~defproblem _SATZ25_THM (in landau1)
 (conclusion conc _SATZ25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_325_T2 _SOMEAPP)

"))

(th~defproblem _SATZ25A_THM (in landau1)
 (conclusion conc _SATZ25A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ25 _SATZ4A _ISMOREIS2)

"))

(th~defproblem _SATZ25B_THM (in landau1)
 (conclusion conc _SATZ25B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ25 _SATZ13)

"))

(th~defproblem _SATZ25C_THM (in landau1)
 (conclusion conc _SATZ25C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ25B _SATZ4A _ISLESSIS1)

"))

(th~defproblem _326_T1_THM (in landau1)
 (conclusion conc _326_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ25)

"))

(th~defproblem _326_T2_THM (in landau1)
 (conclusion conc _326_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_326_T1 _SATZ10H _TH3)

"))

(th~defproblem _SATZ26_THM (in landau1)
 (conclusion conc _SATZ26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_326_T2 _SATZ10E)

"))

(th~defproblem _SATZ26A_THM (in landau1)
 (conclusion conc _SATZ26A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ4E _ISLESS2 _SATZ26)

"))

(th~defproblem _SATZ26B_THM (in landau1)
 (conclusion conc _SATZ26B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ26 _SATZ14)

"))

(th~defproblem _SATZ26C_THM (in landau1)
 (conclusion conc _SATZ26C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ4E _ISMORE1 _SATZ26B)

"))

(th~defproblem _327_T1_THM (in landau1)
 (conclusion conc _327_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ24A)

"))

(th~defproblem _327_T2_THM (in landau1)
 (conclusion conc _327_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T1)

"))

(th~defproblem _327_T3_THM (in landau1)
 (conclusion conc _327_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_SATZ18)

"))

(th~defproblem _327_T4_THM (in landau1)
 (conclusion conc _327_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T3 _SATZ10G)

"))

(th~defproblem _327_T5_THM (in landau1)
 (conclusion conc _327_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T4 _TH4)

"))

(th~defproblem _327_T6_THM (in landau1)
 (conclusion conc _327_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T5 _ALL_TH1)

"))

(th~defproblem _327_T7_THM (in landau1)
 (conclusion conc _327_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T6 _MP)

"))

(th~defproblem _327_T8_THM (in landau1)
 (conclusion conc _327_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T7 _SOMEAPP)

"))

(th~defproblem _327_T9_THM (in landau1)
 (conclusion conc _327_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _327_T10_THM (in landau1)
 (conclusion conc _327_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_327_T9 _AND_TH3 _ET)

"))

(th~defproblem _327_T11_THM (in landau1)
 (conclusion conc _327_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4A _327_T10 _ISP)

"))

(th~defproblem _327_T12_THM (in landau1)
 (conclusion conc _327_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T11 _327_T2 _INDUCTION)

"))

(th~defproblem _327_T13_THM (in landau1)
 (conclusion conc _327_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T12 _327_T8)

"))

(th~defproblem _327_T14_THM (in landau1)
 (conclusion conc _327_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _327_T15_THM (in landau1)
 (conclusion conc _327_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _327_T16_THM (in landau1)
 (conclusion conc _327_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_327_T14 _MP)

"))

(th~defproblem _327_T17_THM (in landau1)
 (conclusion conc _327_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_SYMIS _ISP _TH3)

"))

(th~defproblem _327_T18_THM (in landau1)
 (conclusion conc _327_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_327_T17 _327_T16 _ORE1)

"))

(th~defproblem _327_T19_THM (in landau1)
 (conclusion conc _327_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T18 _SATZ25B)

"))

(th~defproblem _327_T20_THM (in landau1)
 (conclusion conc _327_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T19)

"))

(th~defproblem _327_T21_THM (in landau1)
 (conclusion conc _327_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T15 _327_T20 _MP)

"))

(th~defproblem _327_T22_THM (in landau1)
 (conclusion conc _327_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T21 _ET)

"))

(th~defproblem _327_T23_THM (in landau1)
 (conclusion conc _327_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T22 _327_T14 _ANDI)

"))

(th~defproblem _SATZ27_THM (in landau1)
 (conclusion conc _SATZ27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T23 _327_T13 _SOME_TH6)

"))

(th~defproblem _327_T24_THM (in landau1)
 (conclusion conc _327_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ24A)

"))

(th~defproblem _327_T25_THM (in landau1)
 (conclusion conc _327_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T24)

"))

(th~defproblem _327_T26_THM (in landau1)
 (conclusion conc _327_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _327_T27_THM (in landau1)
 (conclusion conc _327_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_327_T26 _AND_TH3)

"))

(th~defproblem _327_T28_THM (in landau1)
 (conclusion conc _327_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISP1 _327_T27 _TH3)

"))

(th~defproblem _327_T29_THM (in landau1)
 (conclusion conc _327_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_MP)

"))

(th~defproblem _327_T30_THM (in landau1)
 (conclusion conc _327_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_327_T28 _327_T29 _ORE1)

"))

(th~defproblem _327_T31_THM (in landau1)
 (conclusion conc _327_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T30 _SATZ25C)

"))

(th~defproblem _327_T32_THM (in landau1)
 (conclusion conc _327_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T31)

"))

(th~defproblem _327_T33_THM (in landau1)
 (conclusion conc _327_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T32)

"))

(th~defproblem _327_T34_THM (in landau1)
 (conclusion conc _327_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_327_T33 _327_T25 _INDUCTION)

"))

(th~defproblem _327_T35_THM (in landau1)
 (conclusion conc _327_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ18B _SATZ10G)

"))

(th~defproblem _327_T36_THM (in landau1)
 (conclusion conc _327_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T35 _TH4)

"))

(th~defproblem _T37_THM (in landau1)
 (conclusion conc _T37)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_327_T36 _ALL_TH1)

"))

(th~defproblem _T38_THM (in landau1)
 (conclusion conc _T38)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T37 _327_T34 _MP)

"))

(th~defproblem _327_ANDERS_THM (in landau1)
 (conclusion conc _327_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T38 _SOMEAPP)

"))

(th~defproblem _T39_THM (in landau1)
 (conclusion conc _T39)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _T40_THM (in landau1)
 (conclusion conc _T40)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _T41_THM (in landau1)
 (conclusion conc _T41)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _T42_THM (in landau1)
 (conclusion conc _T42)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _T43_THM (in landau1)
 (conclusion conc _T43)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_T39)

"))

(th~defproblem _T44_THM (in landau1)
 (conclusion conc _T44)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_T40)

"))

(th~defproblem _T45_THM (in landau1)
 (conclusion conc _T45)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_T43 _T42 _MP)

"))

(th~defproblem _T46_THM (in landau1)
 (conclusion conc _T46)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_T44 _T41 _MP)

"))

(th~defproblem _T47_THM (in landau1)
 (conclusion conc _T47)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T45 _SATZ10D _T46 _SATZ14 _ORE2)

"))

(th~defproblem _T48_THM (in landau1)
 (conclusion conc _T48)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T47)

"))

(th~defproblem _SATZ27A_THM (in landau1)
 (conclusion conc _SATZ27A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ27 _T48 _ONEI)

"))

(th~defproblem _428_T1_THM (in landau1)
 (conclusion conc _428_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _428_T2_THM (in landau1)
 (conclusion conc _428_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _428_T3_THM (in landau1)
 (conclusion conc _428_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T2 _428_T1 _TRIS2)

"))

(th~defproblem _428_T4_THM (in landau1)
 (conclusion conc _428_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISPL1)

"))

(th~defproblem _428_T5_THM (in landau1)
 (conclusion conc _428_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _428_T6_THM (in landau1)
 (conclusion conc _428_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _428_T7_THM (in landau1)
 (conclusion conc _428_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T5)

"))

(th~defproblem _428_T8_THM (in landau1)
 (conclusion conc _428_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T6)

"))

(th~defproblem _428_T9_THM (in landau1)
 (conclusion conc _428_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T8 _SYMIS _428_T4 _428_T7 _TR3IS)

"))

(th~defproblem _428_T10_THM (in landau1)
 (conclusion conc _428_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T9 _428_T3 _INDUCTION)

"))

(th~defproblem _428_T11_THM (in landau1)
 (conclusion conc _428_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T10 _FISI)

"))

(th~defproblem _A1_THM (in landau1)
 (conclusion conc _A1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T11)

"))

(th~defproblem _428_T12_THM (in landau1)
 (conclusion conc _428_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4E)

"))

(th~defproblem _428_T13_THM (in landau1)
 (conclusion conc _428_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T12 _REFIS _ANDI)

"))

(th~defproblem _428_T14_THM (in landau1)
 (conclusion conc _428_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T13 _SOMEI)

"))

(th~defproblem _428_T15_THM (in landau1)
 (conclusion conc _428_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _428_T16_THM (in landau1)
 (conclusion conc _428_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ4A _428_T15 _ISPL1 _TRIS)

"))

(th~defproblem _428_T17_THM (in landau1)
 (conclusion conc _428_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _428_T18_THM (in landau1)
 (conclusion conc _428_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_428_T17)

"))

(th~defproblem _428_T19_THM (in landau1)
 (conclusion conc _428_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSPL1 _428_T18 _ISPL1 _TRIS)

"))

(th~defproblem _428_T20_THM (in landau1)
 (conclusion conc _428_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMPL _SATZ4H _SATZ4B _TR3IS)

"))

(th~defproblem _428_T21_THM (in landau1)
 (conclusion conc _428_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSPL2 _428_T20 _ISPL2 _428_T19 _TR3IS)

"))

(th~defproblem _428_T22_THM (in landau1)
 (conclusion conc _428_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T21)

"))

(th~defproblem _428_T23_THM (in landau1)
 (conclusion conc _428_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T22 _428_T16 _ANDI)

"))

(th~defproblem _428_T24_THM (in landau1)
 (conclusion conc _428_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T23 _SOMEI)

"))

(th~defproblem _428_T25_THM (in landau1)
 (conclusion conc _428_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T24 _SOMEAPP)

"))

(th~defproblem _B1_THM (in landau1)
 (conclusion conc _B1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T25 _428_T14 _INDUCTION)

"))

(th~defproblem _SATZ28_THM (in landau1)
 (conclusion conc _SATZ28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_B1 _A1 _ONEI)

"))

(th~defproblem _428_T26_THM (in landau1)
 (conclusion conc _428_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28)

"))

(th~defproblem _SATZ28A_THM (in landau1)
 (conclusion conc _SATZ28A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T26 _ANDE1)

"))

(th~defproblem _428_T27_THM (in landau1)
 (conclusion conc _428_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T26 _ANDE2)

"))

(th~defproblem _SATZ28B_THM (in landau1)
 (conclusion conc _SATZ28B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T27)

"))

(th~defproblem _428_T28_THM (in landau1)
 (conclusion conc _428_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T13 _428_T26 _428_T11)

"))

(th~defproblem _SATZ28C_THM (in landau1)
 (conclusion conc _SATZ28C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T28 _FISE)

"))

(th~defproblem _428_T29_THM (in landau1)
 (conclusion conc _428_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T26 _B1 _428_T23 _428_T11)

"))

(th~defproblem _SATZ28D_THM (in landau1)
 (conclusion conc _SATZ28D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_428_T29 _FISE)

"))

(th~defproblem _SATZ28E_THM (in landau1)
 (conclusion conc _SATZ28E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28A _SYMIS)

"))

(th~defproblem _SATZ28F_THM (in landau1)
 (conclusion conc _SATZ28F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28B _SYMIS)

"))

(th~defproblem _SATZ28G_THM (in landau1)
 (conclusion conc _SATZ28G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28C _SYMIS)

"))

(th~defproblem _SATZ28H_THM (in landau1)
 (conclusion conc _SATZ28H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28D _SYMIS)

"))

(th~defproblem _N_ISTS1_THM (in landau1)
 (conclusion conc _N_ISTS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _N_ISTS2_THM (in landau1)
 (conclusion conc _N_ISTS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _ISTS12_THM (in landau1)
 (conclusion conc _ISTS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_N_ISTS2 _N_ISTS1 _TRIS)

"))

(th~defproblem _429_T1_THM (in landau1)
 (conclusion conc _429_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28A)

"))

(th~defproblem _429_T2_THM (in landau1)
 (conclusion conc _429_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28C)

"))

(th~defproblem _429_T3_THM (in landau1)
 (conclusion conc _429_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_429_T1 _429_T2 _TRIS2)

"))

(th~defproblem _429_T4_THM (in landau1)
 (conclusion conc _429_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28F _ISPL1 _TRIS)

"))

(th~defproblem _429_T5_THM (in landau1)
 (conclusion conc _429_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28D)

"))

(th~defproblem _429_T6_THM (in landau1)
 (conclusion conc _429_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_429_T4 _429_T5 _TRIS)

"))

(th~defproblem _SATZ29_THM (in landau1)
 (conclusion conc _SATZ29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_429_T6 _429_T3 _INDUCTION)

"))

(th~defproblem _COMTS_THM (in landau1)
 (conclusion conc _COMTS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ29)

"))

(th~defproblem _429_T7_THM (in landau1)
 (conclusion conc _429_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28G _SATZ28A _TRIS)

"))

(th~defproblem _429_T8_THM (in landau1)
 (conclusion conc _429_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28H _ISPL1 _SATZ28B _TR3IS)

"))

(th~defproblem _429_ANDERS_THM (in landau1)
 (conclusion conc _429_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_429_T8 _429_T7 _INDUCTION)

"))

(th~defproblem _430_T1_THM (in landau1)
 (conclusion conc _430_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28E _ISPL2 _SATZ28B _SATZ4A _N_ISTS2 _TR3IS)

"))

(th~defproblem _430_T2_THM (in landau1)
 (conclusion conc _430_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISPL1 _SATZ28B _SATZ4B _N_ISTS2 _TR3IS)

"))

(th~defproblem _430_T3_THM (in landau1)
 (conclusion conc _430_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28F _ISPL2 _ASSPL1 _430_T2 _TR3IS)

"))

(th~defproblem _SATZ30_THM (in landau1)
 (conclusion conc _SATZ30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_430_T3 _430_T1 _INDUCTION)

"))

(th~defproblem _DISTTP1_THM (in landau1)
 (conclusion conc _DISTTP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _ISPL12 _SATZ30 _TR3IS)

"))

(th~defproblem _DISTTP2_THM (in landau1)
 (conclusion conc _DISTTP2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ30)

"))

(th~defproblem _DISTPT1_THM (in landau1)
 (conclusion conc _DISTPT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_DISTTP1 _SYMIS)

"))

(th~defproblem _DISTPT2_THM (in landau1)
 (conclusion conc _DISTPT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_DISTTP2 _SYMIS)

"))

(th~defproblem _431_T1_THM (in landau1)
 (conclusion conc _431_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28E _N_ISTS2 _SATZ28A _TRIS)

"))

(th~defproblem _431_T2_THM (in landau1)
 (conclusion conc _431_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28F _N_ISTS2 _DISTPT2 _ISPL1 _SATZ28B _TR4IS)

"))

(th~defproblem _SATZ31_THM (in landau1)
 (conclusion conc _SATZ31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_431_T2 _431_T1 _INDUCTION)

"))

(th~defproblem _ASSTS1_THM (in landau1)
 (conclusion conc _ASSTS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ31)

"))

(th~defproblem _ASSTS2_THM (in landau1)
 (conclusion conc _ASSTS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSTS1 _SYMIS)

"))

(th~defproblem _432_T1_THM (in landau1)
 (conclusion conc _432_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_DISTTP1 _N_ISTS1 _TRIS)

"))

(th~defproblem _432_T2_THM (in landau1)
 (conclusion conc _432_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_432_T1 _SOMEI)

"))

(th~defproblem _SATZ32A_THM (in landau1)
 (conclusion conc _SATZ32A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_432_T2 _SOMEAPP)

"))

(th~defproblem _SATZ32B_THM (in landau1)
 (conclusion conc _SATZ32B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_N_ISTS1)

"))

(th~defproblem _SATZ32C_THM (in landau1)
 (conclusion conc _SATZ32C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ12 _SATZ32A _SATZ11)

"))

(th~defproblem _432_ANDERS1_THM (in landau1)
 (conclusion conc _432_ANDERS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32A)

"))

(th~defproblem _SATZ32D_THM (in landau1)
 (conclusion conc _SATZ32D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32A _COMTS _ISMORE12)

"))

(th~defproblem _SATZ32E_THM (in landau1)
 (conclusion conc _SATZ32E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_N_ISTS2)

"))

(th~defproblem _SATZ32F_THM (in landau1)
 (conclusion conc _SATZ32F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32C _COMTS _ISLESS12)

"))

(th~defproblem _432_ANDERS2_THM (in landau1)
 (conclusion conc _432_ANDERS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32D)

"))

(th~defproblem _SATZ32G_THM (in landau1)
 (conclusion conc _SATZ32G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32D _N_ISTS1 _ISMORE2)

"))

(th~defproblem _SATZ32H_THM (in landau1)
 (conclusion conc _SATZ32H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32G _COMTS _ISMORE12)

"))

(th~defproblem _SATZ32J_THM (in landau1)
 (conclusion conc _SATZ32J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32F _N_ISTS1 _ISLESS2)

"))

(th~defproblem _SATZ32K_THM (in landau1)
 (conclusion conc _SATZ32K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32J _COMTS _ISLESS12)

"))

(th~defproblem _432_T3_THM (in landau1)
 (conclusion conc _432_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32A _MOREISI1)

"))

(th~defproblem _432_T4_THM (in landau1)
 (conclusion conc _432_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_N_ISTS1 _MOREISI2)

"))

(th~defproblem _SATZ32L_THM (in landau1)
 (conclusion conc _SATZ32L)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_432_T4 _432_T3 _ORAPP)

"))

(th~defproblem _SATZ32M_THM (in landau1)
 (conclusion conc _SATZ32M)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32L _COMTS _ISMOREIS12)

"))

(th~defproblem _SATZ32N_THM (in landau1)
 (conclusion conc _SATZ32N)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ32L _SATZ13)

"))

(th~defproblem _SATZ32O_THM (in landau1)
 (conclusion conc _SATZ32O)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ32M _SATZ13)

"))

(th~defproblem _433_T1_THM (in landau1)
 (conclusion conc _433_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ10A)

"))

(th~defproblem _433_T2_THM (in landau1)
 (conclusion conc _433_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ10B)

"))

(th~defproblem _SATZ33A_THM (in landau1)
 (conclusion conc _SATZ33A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ32C _SATZ32A _SATZ32B _433_T2 _433_T1 _EC3_TH11)

"))

(th~defproblem _SATZ33B_THM (in landau1)
 (conclusion conc _SATZ33B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ32C _SATZ32A _SATZ32B _433_T2 _433_T1 _EC3_TH10)

"))

(th~defproblem _SATZ33C_THM (in landau1)
 (conclusion conc _SATZ33C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ32C _SATZ32A _SATZ32B _433_T2 _433_T1 _EC3_TH12)

"))

(th~defproblem _433_ANDERS_THM (in landau1)
 (conclusion conc _433_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ33A)

"))

(th~defproblem _434_T1_THM (in landau1)
 (conclusion conc _434_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32A)

"))

(th~defproblem _434_T2_THM (in landau1)
 (conclusion conc _434_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32A _COMTS _ISMORE12)

"))

(th~defproblem _SATZ34_THM (in landau1)
 (conclusion conc _SATZ34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_434_T2 _434_T1 _TRMORE)

"))

(th~defproblem _434_ANDERS_THM (in landau1)
 (conclusion conc _434_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32D _SATZ32A _TRMORE)

"))

(th~defproblem _SATZ34A_THM (in landau1)
 (conclusion conc _SATZ34A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ34)

"))

(th~defproblem _434_ANDERSA_THM (in landau1)
 (conclusion conc _434_ANDERSA)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ12 _SATZ34 _SATZ11)

"))

(th~defproblem _SATZ35A_THM (in landau1)
 (conclusion conc _SATZ35A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32G _SATZ34 _ORAPP)

"))

(th~defproblem _SATZ35B_THM (in landau1)
 (conclusion conc _SATZ35B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32H _SATZ34 _ORAPP)

"))

(th~defproblem _SATZ35C_THM (in landau1)
 (conclusion conc _SATZ35C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ35A)

"))

(th~defproblem _SATZ35D_THM (in landau1)
 (conclusion conc _SATZ35D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ35B)

"))

(th~defproblem _436_T1_THM (in landau1)
 (conclusion conc _436_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_N_ISTS2 _N_ISTS1 _TRIS _MOREISI2)

"))

(th~defproblem _436_T2_THM (in landau1)
 (conclusion conc _436_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ35A _MOREISI1)

"))

(th~defproblem _436_T3_THM (in landau1)
 (conclusion conc _436_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_436_T1 _436_T2 _ORAPP)

"))

(th~defproblem _436_T4_THM (in landau1)
 (conclusion conc _436_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ35B _MOREISI1)

"))

(th~defproblem _SATZ36_THM (in landau1)
 (conclusion conc _SATZ36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_436_T3 _436_T4 _ORAPP)

"))

(th~defproblem _436_T5_THM (in landau1)
 (conclusion conc _436_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ35B _MOREISI1)

"))

(th~defproblem _436_T6_THM (in landau1)
 (conclusion conc _436_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32M _N_ISTS1 _ISMOREIS2)

"))

(th~defproblem _436_ANDERS_THM (in landau1)
 (conclusion conc _436_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_436_T6 _436_T5 _ORAPP)

"))

(th~defproblem _SATZ36A_THM (in landau1)
 (conclusion conc _SATZ36A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ14 _SATZ36 _SATZ13)

"))

(th~defproblem _MN_T1_THM (in landau1)
 (conclusion conc _MN_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_SATZ8B _ONEI)

"))

(th~defproblem _TH1A_THM (in landau1)
 (conclusion conc _TH1A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_MN_T1)

"))

(th~defproblem _TH1B_THM (in landau1)
 (conclusion conc _TH1B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_TH1A _SYMIS)

"))

(th~defproblem _TH1C_THM (in landau1)
 (conclusion conc _TH1C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_COMPL _TH1A _TRIS)

"))

(th~defproblem _TH1D_THM (in landau1)
 (conclusion conc _TH1D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_TH1C _SYMIS)

"))

(th~defproblem _TH1E_THM (in landau1)
 (conclusion conc _TH1E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_TH1A _SYMIS _SATZ8B)

"))

(th~defproblem _MN_T2_THM (in landau1)
 (conclusion conc _MN_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_TH1B _SYMIS _ISPL1 _TR3IS)

"))

(th~defproblem _ISMN12_THM (in landau1)
 (conclusion conc _ISMN12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_MN_T2 _TH1E)

"))

(th~defproblem _1TOP_THM (in landau1)
 (conclusion conc _1TOP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_INP)

"))

(th~defproblem _ISOUTNI_THM (in landau1)
 (conclusion conc _ISOUTNI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISOUTI)

"))

(th~defproblem _ISOUTNE_THM (in landau1)
 (conclusion conc _ISOUTNE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISOUTE)

"))

(th~defproblem _ISINNI_THM (in landau1)
 (conclusion conc _ISINNI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISINI)

"))

(th~defproblem _ISINNE_THM (in landau1)
 (conclusion conc _ISINNE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_ISINE)

"))

(th~defproblem _ISOUTINN_THM (in landau1)
 (conclusion conc _ISOUTINN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISOUTIN _1TOP)

"))

(th~defproblem _ISINOUTN_THM (in landau1)
 (conclusion conc _ISINOUTN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISINOUT)

"))

(th~defproblem _SINGLET_T1_THM (in landau1)
 (conclusion conc _SINGLET_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_1TOP)

"))

(th~defproblem _SINGLET_T2_THM (in landau1)
 (conclusion conc _SINGLET_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SINGLET_T1 _SATZ10D _SATZ24 _ORE2)

"))

(th~defproblem _SINGLET_TH1_THM (in landau1)
 (conclusion conc _SINGLET_TH1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SINGLET_T2 _REFIS _LESSISI2 _SINGLET_T1 _ISOUTNI _ISOUTINN _TRIS)

"))

(th~defproblem _PAIR_T1_THM (in landau1)
 (conclusion conc _PAIR_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORE1 _SATZ26)

"))

(th~defproblem _PAIR_T2_THM (in landau1)
 (conclusion conc _PAIR_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR_T1 _SATZ10D _SATZ24 _ORE2)

"))

(th~defproblem _PAIR_TH1_THM (in landau1)
 (conclusion conc _PAIR_TH1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR_T2 _OR_TH2)

"))

(th~defproblem _PAIR_TH2_THM (in landau1)
 (conclusion conc _PAIR_TH2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ4E _NOTIS_TH1)

"))

(th~defproblem _PAIR_T3_THM (in landau1)
 (conclusion conc _PAIR_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_1TOP)

"))

(th~defproblem _PAIR_T4_THM (in landau1)
 (conclusion conc _PAIR_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ24A _PAIR_T3 _ISOUTNI)

"))

(th~defproblem _PAIR_T5_THM (in landau1)
 (conclusion conc _PAIR_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_PAIR_T4 _ISOUTINN _PAIR_T3 _TRIS)

"))

(th~defproblem _PAIR_T6_THM (in landau1)
 (conclusion conc _PAIR_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_REFIS _LESSISI2 _PAIR_T3 _ISOUTNI)

"))

(th~defproblem _PAIR_T7_THM (in landau1)
 (conclusion conc _PAIR_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_PAIR_T6 _ISOUTINN _PAIR_T3 _TRIS)

"))

(th~defproblem _PAIR_TH3_THM (in landau1)
 (conclusion conc _PAIR_TH3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR_T7 _PAIR_T5 _PAIR_T3 _PAIR_TH1 _TH9)

"))

(th~defproblem _PAIR_T9_THM (in landau1)
 (conclusion conc _PAIR_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISINI)

"))

(th~defproblem _PAIR_T10_THM (in landau1)
 (conclusion conc _PAIR_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ24A _ISINOUTN _SYMIS _PAIR_T9 _REFIS _LESSISI2 _TR3IS)

"))

(th~defproblem _PAIR_TH4_THM (in landau1)
 (conclusion conc _PAIR_TH4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PAIR_T10 _PAIR_TH2 _TH3)

"))

(th~defproblem _FIRST1IS1_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_FIRST1IS1 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_REFIS _ITET)

"))

(th~defproblem _FIRST1IS2_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_FIRST1IS2 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_FIRST1IS1 _SYMIS)

"))

(th~defproblem _SECOND1IS1_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_SECOND1IS1 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PAIR_TH4 _ITEF)

"))

(th~defproblem _SECOND1IS2_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_SECOND1IS2 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SECOND1IS1 _SYMIS)

"))

(th~defproblem _PAIR_T11_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T11 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _PAIR_T12_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T12 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF _SYMIS)

"))

(th~defproblem _PAIR_T13_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T13 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_PAIR_T12 _PAIR_T11 _TR3IS)

"))

(th~defproblem _PAIR_T14_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T14 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _PAIR_T15_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T15 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF _SYMIS)

"))

(th~defproblem _PAIR_T16_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T16 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_PAIR_T15 _PAIR_T14 _TR3IS)

"))

(th~defproblem _PAIR_T17_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T17 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR_T16 _PAIR_T13 _PAIR_TH3 _ORAPP)

"))

(th~defproblem _PAIR_TH5_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_TH5 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR_T17 _FISI)

"))

(th~defproblem _PAIR_T18_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T18 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_FIRST1IS1)

"))

(th~defproblem _PAIR_T19_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR_T19 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SECOND1IS1)

"))

(th~defproblem _PAIR1IS1_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR1IS1 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR_T19 _PAIR_T18 _PAIR_TH5)

"))

(th~defproblem _PAIR1IS2_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_PAIR1IS2 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR1IS1 _SYMIS)

"))

(th~defproblem _LESSISI3_THM (in landau1)
 (conclusion conc _LESSISI3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_REFIS _LESSISI2)

"))

(th~defproblem _LEFT_T1_THM (in landau1)
 (conclusion conc _LEFT_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_1TOP)

"))

(th~defproblem _LEFT_T2_THM (in landau1)
 (conclusion conc _LEFT_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_LEFT_T1 _TRLESSIS)

"))

(th~defproblem _LEFT_T3_THM (in landau1)
 (conclusion conc _LEFT_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_LEFT_T2 _ISOUTNE)

"))

(th~defproblem _THLEFT1_THM (in landau1)
 (conclusion conc _THLEFT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_LEFT_T3 _ISINNE)

"))

(th~defproblem _THLEFT2_THM (in landau1)
 (conclusion conc _THLEFT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_THLEFT1)

"))

(th~defproblem _RIGHT_T4_THM (in landau1)
 (conclusion conc _RIGHT_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_1TOP)

"))

(th~defproblem _RIGHT_T5_THM (in landau1)
 (conclusion conc _RIGHT_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_RIGHT_T4 _SATZ19O)

"))

(th~defproblem _RIGHT_T6_THM (in landau1)
 (conclusion conc _RIGHT_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_RIGHT_T5 _ISOUTNE)

"))

(th~defproblem _RIGHT_T7_THM (in landau1)
 (conclusion conc _RIGHT_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RIGHT_T6 _SATZ20E)

"))

(th~defproblem _THRIGHT1_THM (in landau1)
 (conclusion conc _THRIGHT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RIGHT_T7 _ISINNE)

"))

(th~defproblem _LEFT_T4_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_LEFT_T4 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_LESSISI2)

"))

(th~defproblem _LEFT_T5_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_LEFT_T5 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_AX5)
Should be provable from hyps (_SYMIS _LESSISI2)

"))

(th~defproblem _LEFT_T6_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_LEFT_T6 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_LEFT_T4 _1TOP _TRLESSIS _ISINOUTN)

"))

(th~defproblem _LEFT_T7_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_LEFT_T7 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_LEFT_T6 _LEFT_T5 _LEFT_T4 _1TOP _TRLESSIS _ISOUTNI _ISOUTINN _TRIS)

"))

(th~defproblem _LEFT_T8_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_LEFT_T8 DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_LEFT_T7 _LEFT_T4 _LEFT_T5 _ISF)

"))

(th~defproblem _THLEFT_THM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_THLEFT DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_LEFT_T8 _FISI _SYMIS _LESSISI2)

"))

(th~defproblem _I1_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _I1_T3 _I1_T1) _I1_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ1_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _21_T1 _TH3) _SATZ1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _22_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ1 _22_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _22_T2 _22_T1) _INDUCTION) _SATZ2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _23_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _23_T2 _ORI2) _23_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _23_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _23_T3 _23_T1) _INDUCTION) _23_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _23_T4 _ORE2) _SATZ3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _23_T6_HTHM (in landau1)
 (conclusion conc (IMPLIES _23_T5 _23_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _24_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _24_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES _AX2 _24_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _24_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T6_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _24_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T7_HTHM (in landau1)
 (conclusion conc (IMPLIES _24_T5 _24_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T8_HTHM (in landau1)
 (conclusion conc (IMPLIES _24_T6 _24_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T10_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _24_T9 _24_T3) _INDUCTION) _24_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _AA_HTHM (in landau1)
 (conclusion conc (IMPLIES _24_T11 _AA))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T16_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _24_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T18_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _24_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T19_HTHM (in landau1)
 (conclusion conc (IMPLIES _24_T18 _24_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T22_HTHM (in landau1)
 (conclusion conc (IMPLIES _24_T21 _24_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T23_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _24_T22 _24_T17) _ANDI) _24_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _BB_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _24_T25 _24_T14) _INDUCTION) _BB))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T26_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ4 _24_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ4A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _24_T26 _ANDE1) _SATZ4A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T27_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _24_T26 _ANDE2) _24_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ4B_HTHM (in landau1)
 (conclusion conc (IMPLIES _24_T27 _SATZ4B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T28_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _24_T13 _24_T26) _24_T11) _24_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _24_T29_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _24_T26 _BB) _24_T23) _24_T11) _24_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _25_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _AX2 _25_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ5_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _25_T3 _25_T1) _INDUCTION) _SATZ5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ASSPL1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ5 _ASSPL1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _26_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ4A _26_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _26_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ4C _26_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _26_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ4D _26_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ6_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _26_T6 _26_T3) _INDUCTION) _SATZ6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _COMPL_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ6 _COMPL))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _26_T8 _26_T7) _INDUCTION) _ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _27_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ1 _27_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ7_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _27_T4 _27_T2) _INDUCTION) _SATZ7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _28_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ1 _28_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _28_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ1 _28_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ8_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _28_T4 _28_T2) _INDUCTION) _SATZ8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ8A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ8 _WELI) _TH7) _SATZ8A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ8B_HTHM (in landau1)
 (conclusion conc (IMPLIES _28_T5 _SATZ8B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T3 _EC_TH1) _29_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T6_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T5 _EC_TH2) _29_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T7_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ7 _T6A) _MP) _29_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T10_HTHM (in landau1)
 (conclusion conc (IMPLIES _29_T9 _29_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T11_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T10 _EC_TH1) _29_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _29_T6 _29_T11) _29_T4) _EC3_TH6) _A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T12_HTHM (in landau1)
 (conclusion conc (IMPLIES _OR3I1 _29_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T16_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T15 _OR3I2) _29_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T17_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _T16A _29_T12) _TH1) _29_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T20_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T19 _OR3I3) _29_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T22_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T21 _OR3I1) _29_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T27_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T26 _OR3I2) _29_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T28_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _29_T27 _29_T22) _TH1) _29_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T32_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _29_T31 _OR3I3) _29_T32))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _29_T33_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _29_T32 _T28A) _29_T20) _OR3APP) _29_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _29_T33 _29_T17) _INDUCTION) _B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ9_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _A _B) _OREC3I) _SATZ9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ9A_HTHM (in landau1)
 (conclusion conc (IMPLIES _B _SATZ9A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ9B_HTHM (in landau1)
 (conclusion conc (IMPLIES _A _SATZ9B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ9 _SATZ10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10A_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ9A _SATZ10A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10B_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ9B _SATZ10B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREISI2_HTHM (in landau1)
 (conclusion conc (IMPLIES _ORI2 _MOREISI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSISI2_HTHM (in landau1)
 (conclusion conc (IMPLIES _ORI2 _LESSISI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREISI1_HTHM (in landau1)
 (conclusion conc (IMPLIES _ORI1 _MOREISI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSISI1_HTHM (in landau1)
 (conclusion conc (IMPLIES _ORI1 _LESSISI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISMORE12_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _ISMORE1 _ISMORE2) _ISMORE12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISLESS12_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _ISLESS1 _ISLESS2) _ISLESS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISMOREIS12_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _ISMOREIS1 _ISMOREIS2) _ISMOREIS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISLESSIS12_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _ISLESSIS1 _ISLESSIS2) _ISLESSIS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _COMOR _SATZ10B) _EC3_TH7) _SATZ10C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10D_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ10B _EC3_TH9) _SATZ10D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10E_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ10A _OR3_TH2) _SATZ10E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10F_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ10A _OR3_TH3) _COMOR) _SATZ10F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10G_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ10B _EC3E21) _EC3E23) _OR_TH3) _SATZ10G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10H_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ10B _EC3E31) _EC3E32) _OR_TH3) _SATZ10H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10J_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH4 _OR_TH5) _SATZ10A) _OR3E3) _SATZ10J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ10K_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH5 _OR_TH4) _SATZ10A) _OR3E2) _SATZ10K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRLESS_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ15 _TRLESS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRMORE_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ15 _TRMORE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _315_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ11 _SATZ15) _SATZ12) _315_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ16B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _ISLESS2 _TRLESS) _ORAPP) _SATZ16B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ16C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ13 _SATZ16B) _SATZ16C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ16D_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ13 _SATZ16A) _SATZ16D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _317_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ16A _LESSISI1) _317_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _317_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _317_T1 _317_T2) _ORAPP) _317_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _317_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ16B _LESSISI1) _317_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ17_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _317_T3 _317_T4) _ORAPP) _SATZ17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _317_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ16B _LESSISI1) _317_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _317_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _317_T6 _317_T5) _ORAPP) _317_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRLESSIS_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ17 _TRLESSIS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRMOREIS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ13 _SATZ17) _SATZ14) _TRMOREIS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ18A_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ18 _SATZ18A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ18B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ18 _SATZ4A) _ISMORE1) _SATZ18B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ18C_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ18B _SATZ18C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19B_HTHM (in landau1)
 (conclusion conc (IMPLIES _ISPL1 _SATZ19B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ12 _SATZ19A) _SATZ11) _SATZ19C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ANDERS1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ19A _ANDERS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19D_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19A _COMPL) _ISMORE12) _SATZ19D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19E_HTHM (in landau1)
 (conclusion conc (IMPLIES _ISPL2 _SATZ19E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19F_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19C _COMPL) _ISLESS12) _SATZ19F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ANDERS2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ19D _ANDERS2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19G_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19D _ISPL1) _ISMORE2) _SATZ19G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19H_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19G _COMPL) _ISMORE12) _SATZ19H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19J_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19F _ISPL1) _ISLESS2) _SATZ19J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19K_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19J _COMPL) _ISLESS12) _SATZ19K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _319_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ19A _MOREISI1) _319_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _319_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _ISPL1 _MOREISI2) _319_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19L_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _319_T5 _319_T4) _ORAPP) _SATZ19L))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19M_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19L _COMPL) _ISMOREIS12) _SATZ19M))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19N_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ14 _SATZ19L) _SATZ13) _SATZ19N))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ19O_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ14 _SATZ19M) _SATZ13) _SATZ19O))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _320_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ10A _320_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _320_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ10B _320_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ20A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ19C _SATZ19A) _SATZ19B) _320_T2) _320_T1) _EC3_TH11) _SATZ20A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ20B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ19C _SATZ19A) _SATZ19B) _320_T2) _320_T1) _EC3_TH10) _SATZ20B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ20C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ19C _SATZ19A) _SATZ19B) _320_T2) _320_T1) _EC3_TH12) _SATZ20C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ANDERSB_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _320_T3 _SATZ8A) _ANDERSB))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ANDERSC_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ20A _ANDERSC))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ20D_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _COMPL _ISMORE12) _SATZ20A) _SATZ20D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ20F_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _COMPL _ISLESS12) _SATZ20C) _SATZ20F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _321_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ19A _321_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _321_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19A _COMPL) _ISMORE12) _321_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ21_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _321_T2 _321_T1) _TRMORE) _SATZ21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _321_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19D _SATZ19A) _TRMORE) _321_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ21A_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ21 _SATZ21A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ANDERSA_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ12 _SATZ21) _SATZ11) _ANDERSA))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ22A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19G _SATZ21) _ORAPP) _SATZ22A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ22B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19H _SATZ21) _ORAPP) _SATZ22B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ22C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ14 _SATZ22A) _SATZ22C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ22D_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ14 _SATZ22B) _SATZ22D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _323_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ22A _MOREISI1) _323_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _323_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _323_T1 _323_T2) _ORAPP) _323_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _323_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ22B _MOREISI1) _323_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ23_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _323_T3 _323_T4) _ORAPP) _SATZ23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _323_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ22B _MOREISI1) _323_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _323_T6_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ19M _ISPL1) _ISMOREIS2) _323_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _323_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _323_T6 _323_T5) _ORAPP) _323_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ23A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ14 _SATZ23) _SATZ13) _SATZ23A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ24_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _324_T3 _OR_TH2) _SATZ24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ24A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ24 _SATZ13) _SATZ24A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ24B_HTHM (in landau1)
 (conclusion conc (IMPLIES _324_T3 _SATZ24B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ24C_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ24B _SATZ24C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _325_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ24 _SATZ19M) _325_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ25A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ25 _SATZ4A) _ISMOREIS2) _SATZ25A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ25B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ25 _SATZ13) _SATZ25B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ25C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ25B _SATZ4A) _ISLESSIS1) _SATZ25C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _326_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ25 _326_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _326_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _326_T1 _SATZ10H) _TH3) _326_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ26_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _326_T2 _SATZ10E) _SATZ26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ26A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ4E _ISLESS2) _SATZ26) _SATZ26A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ26B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ26 _SATZ14) _SATZ26B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ26C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ4E _ISMORE1) _SATZ26B) _SATZ26C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ24A _327_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _327_T1 _327_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ18 _327_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T3 _SATZ10G) _327_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T4 _TH4) _327_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T7_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T6 _MP) _327_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T10_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _327_T9 _AND_TH3) _ET) _327_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T12_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _327_T11 _327_T2) _INDUCTION) _327_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T13_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T12 _327_T8) _327_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T14_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _327_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T15_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _327_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T16_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T14 _MP) _327_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T18_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _327_T17 _327_T16) _ORE1) _327_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T19_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T18 _SATZ25B) _327_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T20_HTHM (in landau1)
 (conclusion conc (IMPLIES _327_T19 _327_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T21_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _327_T15 _327_T20) _MP) _327_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T22_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T21 _ET) _327_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T23_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _327_T22 _327_T14) _ANDI) _327_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T24_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ24A _327_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T25_HTHM (in landau1)
 (conclusion conc (IMPLIES _327_T24 _327_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T27_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T26 _AND_TH3) _327_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T29_HTHM (in landau1)
 (conclusion conc (IMPLIES _MP _327_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T30_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _327_T28 _327_T29) _ORE1) _327_T30))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T31_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T30 _SATZ25C) _327_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T32_HTHM (in landau1)
 (conclusion conc (IMPLIES _327_T31 _327_T32))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T33_HTHM (in landau1)
 (conclusion conc (IMPLIES _327_T32 _327_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T34_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _327_T33 _327_T25) _INDUCTION) _327_T34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T35_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ18B _SATZ10G) _327_T35))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _327_T36_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _327_T35 _TH4) _327_T36))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T38_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _T37 _327_T34) _MP) _T38))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T39_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _T39))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T40_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _T40))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T41_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _T41))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T42_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _T42))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T43_HTHM (in landau1)
 (conclusion conc (IMPLIES _T39 _T43))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T44_HTHM (in landau1)
 (conclusion conc (IMPLIES _T40 _T44))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T45_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _T43 _T42) _MP) _T45))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T46_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _T44 _T41) _MP) _T46))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T47_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _T45 _SATZ10D) _T46) _SATZ14) _ORE2) _T47))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T48_HTHM (in landau1)
 (conclusion conc (IMPLIES _T47 _T48))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _428_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _428_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES _ISPL1 _428_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _428_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T6_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _428_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T7_HTHM (in landau1)
 (conclusion conc (IMPLIES _428_T5 _428_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T8_HTHM (in landau1)
 (conclusion conc (IMPLIES _428_T6 _428_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T10_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _428_T9 _428_T3) _INDUCTION) _428_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _A1_HTHM (in landau1)
 (conclusion conc (IMPLIES _428_T11 _A1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T12_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ4E _428_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T15_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE1 _428_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T17_HTHM (in landau1)
 (conclusion conc (IMPLIES _ANDE2 _428_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T18_HTHM (in landau1)
 (conclusion conc (IMPLIES _428_T17 _428_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T22_HTHM (in landau1)
 (conclusion conc (IMPLIES _428_T21 _428_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T23_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _428_T22 _428_T16) _ANDI) _428_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _B1_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _428_T25 _428_T14) _INDUCTION) _B1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T26_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ28 _428_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ28A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _428_T26 _ANDE1) _SATZ28A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T27_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _428_T26 _ANDE2) _428_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ28B_HTHM (in landau1)
 (conclusion conc (IMPLIES _428_T27 _SATZ28B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T28_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _428_T13 _428_T26) _428_T11) _428_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _428_T29_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _428_T26 _B1) _428_T23) _428_T11) _428_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _429_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ28A _429_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _429_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ28C _429_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _429_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ28D _429_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ29_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _429_T6 _429_T3) _INDUCTION) _SATZ29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _COMTS_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ29 _COMTS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _429_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _429_T8 _429_T7) _INDUCTION) _429_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ30_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _430_T3 _430_T1) _INDUCTION) _SATZ30))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _DISTTP2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ30 _DISTTP2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ31_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _431_T2 _431_T1) _INDUCTION) _SATZ31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ASSTS1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ31 _ASSTS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32B_HTHM (in landau1)
 (conclusion conc (IMPLIES _N_ISTS1 _SATZ32B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ12 _SATZ32A) _SATZ11) _SATZ32C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _432_ANDERS1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ32A _432_ANDERS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32D_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32A _COMTS) _ISMORE12) _SATZ32D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32E_HTHM (in landau1)
 (conclusion conc (IMPLIES _N_ISTS2 _SATZ32E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32F_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32C _COMTS) _ISLESS12) _SATZ32F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _432_ANDERS2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ32D _432_ANDERS2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32G_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32D _N_ISTS1) _ISMORE2) _SATZ32G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32H_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32G _COMTS) _ISMORE12) _SATZ32H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32J_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32F _N_ISTS1) _ISLESS2) _SATZ32J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32K_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32J _COMTS) _ISLESS12) _SATZ32K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _432_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ32A _MOREISI1) _432_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _432_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _N_ISTS1 _MOREISI2) _432_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32L_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _432_T4 _432_T3) _ORAPP) _SATZ32L))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32M_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32L _COMTS) _ISMOREIS12) _SATZ32M))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32N_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ14 _SATZ32L) _SATZ13) _SATZ32N))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ32O_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ14 _SATZ32M) _SATZ13) _SATZ32O))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _433_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ10A _433_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _433_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ10B _433_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ33A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ32C _SATZ32A) _SATZ32B) _433_T2) _433_T1) _EC3_TH11) _SATZ33A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ33B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ32C _SATZ32A) _SATZ32B) _433_T2) _433_T1) _EC3_TH10) _SATZ33B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ33C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ32C _SATZ32A) _SATZ32B) _433_T2) _433_T1) _EC3_TH12) _SATZ33C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _433_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ33A _433_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _434_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ32A _434_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _434_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32A _COMTS) _ISMORE12) _434_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ34_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _434_T2 _434_T1) _TRMORE) _SATZ34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _434_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32D _SATZ32A) _TRMORE) _434_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ34A_HTHM (in landau1)
 (conclusion conc (IMPLIES _SATZ34 _SATZ34A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _434_ANDERSA_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ12 _SATZ34) _SATZ11) _434_ANDERSA))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ35A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32G _SATZ34) _ORAPP) _SATZ35A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ35B_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32H _SATZ34) _ORAPP) _SATZ35B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ35C_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ14 _SATZ35A) _SATZ35C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ35D_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ14 _SATZ35B) _SATZ35D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _436_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ35A _MOREISI1) _436_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _436_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _436_T1 _436_T2) _ORAPP) _436_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _436_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ35B _MOREISI1) _436_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ36_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _436_T3 _436_T4) _ORAPP) _SATZ36))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _436_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _SATZ35B _MOREISI1) _436_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _436_T6_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ32M _N_ISTS1) _ISMOREIS2) _436_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _436_ANDERS_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _436_T6 _436_T5) _ORAPP) _436_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ36A_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ14 _SATZ36) _SATZ13) _SATZ36A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TH1A_HTHM (in landau1)
 (conclusion conc (IMPLIES _MN_T1 _TH1A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISMN12_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _MN_T2 _TH1E) _ISMN12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SINGLET_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _1TOP _SINGLET_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SINGLET_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _SINGLET_T1 _SATZ10D) _SATZ24) _ORE2) _SINGLET_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PAIR_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _ORE1 _SATZ26) _PAIR_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PAIR_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND _PAIR_T1 _SATZ10D) _SATZ24) _ORE2) _PAIR_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PAIR_TH1_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _PAIR_T2 _OR_TH2) _PAIR_TH1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PAIR_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES _1TOP _PAIR_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PAIR_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _SATZ24A _PAIR_T3) _ISOUTNI) _PAIR_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PAIR_TH3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _PAIR_T7 _PAIR_T5) _PAIR_T3) _PAIR_TH1) _TH9) _PAIR_TH3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PAIR_TH4_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND (AND _PAIR_T10 _PAIR_TH2) _TH3) _PAIR_TH4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LEFT_T1_HTHM (in landau1)
 (conclusion conc (IMPLIES _1TOP _LEFT_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LEFT_T2_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _LEFT_T1 _TRLESSIS) _LEFT_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LEFT_T3_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _LEFT_T2 _ISOUTNE) _LEFT_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _THLEFT1_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _LEFT_T3 _ISINNE) _THLEFT1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _THLEFT2_HTHM (in landau1)
 (conclusion conc (IMPLIES _THLEFT1 _THLEFT2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RIGHT_T4_HTHM (in landau1)
 (conclusion conc (IMPLIES _1TOP _RIGHT_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RIGHT_T5_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _RIGHT_T4 _SATZ19O) _RIGHT_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RIGHT_T6_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _RIGHT_T5 _ISOUTNE) _RIGHT_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RIGHT_T7_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _RIGHT_T6 _SATZ20E) _RIGHT_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _THRIGHT1_HTHM (in landau1)
 (conclusion conc (IMPLIES (AND _RIGHT_T7 _ISINNE) _THRIGHT1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LEFT_T4_HTHM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (IMPLIES _LESSISI2 (_LEFT_T4 DUMMY))))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _INDUCTION_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _INDUCTION))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _21_T1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX4 _21_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _22_T1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX3 _22_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _23_T5_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX4 _23_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _SATZ3A_ATHM (in landau1)
 (conclusion conc (IMPLIES (AND _AX4 _AX5) _SATZ3A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _24_T11_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _24_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _SATZ4_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _SATZ4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISPL1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISPL1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISPL2_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISPL2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISPL12_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISPL12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _27_T1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX3 _27_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _SATZ11_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _SATZ11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _SATZ12_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _SATZ12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _SATZ13_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _SATZ13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _SATZ14_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _SATZ14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISMORE1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISMORE1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISMORE2_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISMORE2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISLESS1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISLESS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISLESS2_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISLESS2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISMOREIS1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISMOREIS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISMOREIS2_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISMOREIS2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISLESSIS1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISLESSIS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISLESSIS2_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISLESSIS2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _317_T1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _317_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _317_T6_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _317_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _SATZ18_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _SATZ18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _323_T1_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _323_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _327_T9_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _327_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _327_T17_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _327_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _327_T26_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _327_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _327_T28_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _327_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _428_T3_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _428_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _428_T9_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _428_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _428_T11_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _428_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _1TOP_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _1TOP))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISINNI_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISINNI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _ISINNE_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _ISINNE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _LESSISI3_ATHM (in landau1)
 (conclusion conc (IMPLIES _AX5 _LESSISI3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

(th~defproblem _LEFT_T5_ATHM (in landau1)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (IMPLIES _AX5 (_LEFT_T5 DUMMY))))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen 
relativized using axioms (determined from Automath version)."))

