(th~defproblem _CLCL_THM (in landau3)
 (conclusion conc _CLCL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INP)

"))

(th~defproblem _CLCL1_THM (in landau3)
 (conclusion conc _CLCL1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL _AND3E1)

"))

(th~defproblem _CLCL2_THM (in landau3)
 (conclusion conc _CLCL2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL _AND3E2)

"))

(th~defproblem _CLCL3_THM (in landau3)
 (conclusion conc _CLCL3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL _AND3E3)

"))

(th~defproblem _CLCL1A_THM (in landau3)
 (conclusion conc _CLCL1A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL1 _ANDE1)

"))

(th~defproblem _CLCL1B_THM (in landau3)
 (conclusion conc _CLCL1B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL1 _ANDE2)

"))

(th~defproblem _CUTAPP1A_THM (in landau3)
 (conclusion conc _CUTAPP1A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL1A _NONEMPTYAPP)

"))

(th~defproblem _III1_T1_THM (in landau3)
 (conclusion conc _III1_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL1B _SOME_TH1)

"))

(th~defproblem _CUTAPP1B_THM (in landau3)
 (conclusion conc _CUTAPP1B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T1 _SOMEAPP)

"))

(th~defproblem _III1_T2_THM (in landau3)
 (conclusion conc _III1_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL2 _MP)

"))

(th~defproblem _CUTAPP2A_THM (in landau3)
 (conclusion conc _CUTAPP2A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T2 _MP)

"))

(th~defproblem _CUTAPP2B_THM (in landau3)
 (conclusion conc _CUTAPP2B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A _SATZ83)

"))

(th~defproblem _III1_T3_THM (in landau3)
 (conclusion conc _III1_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CLCL3 _SOME_TH4)

"))

(th~defproblem _III1_T4_THM (in landau3)
 (conclusion conc _III1_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T3 _AND_TH4)

"))

(th~defproblem _III1_T5_THM (in landau3)
 (conclusion conc _III1_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T4 _SOME_TH1)

"))

(th~defproblem _III1_T6_THM (in landau3)
 (conclusion conc _III1_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TH5)

"))

(th~defproblem _III1_T7_THM (in landau3)
 (conclusion conc _III1_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TH6)

"))

(th~defproblem _III1_T8_THM (in landau3)
 (conclusion conc _III1_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T7 _SATZ81J)

"))

(th~defproblem _III1_T9_THM (in landau3)
 (conclusion conc _III1_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T8 _III1_T6)

"))

(th~defproblem _CUTAPP3_THM (in landau3)
 (conclusion conc _CUTAPP3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T9 _III1_T5 _SOMEAPP)

"))

(th~defproblem _III1_T10_THM (in landau3)
 (conclusion conc _III1_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ALL_TH1 _NONEMPTYI _ANDI)

"))

(th~defproblem _III1_T11_THM (in landau3)
 (conclusion conc _III1_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SOME_TH5)

"))

(th~defproblem _CUT1_THM (in landau3)
 (conclusion conc _CUT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T11 _III1_T10 _AND3I)

"))

(th~defproblem _RP_ISE_THM (in landau3)
 (conclusion conc _RP_ISE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINI)

"))

(th~defproblem _ISE1_THM (in landau3)
 (conclusion conc _ISE1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISE _ISSETE1)

"))

(th~defproblem _RP_ISI_THM (in landau3)
 (conclusion conc _RP_ISI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINE)

"))

(th~defproblem _ISI1_THM (in landau3)
 (conclusion conc _ISI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISSETI _RP_ISI)

"))

(th~defproblem _INE_THM (in landau3)
 (conclusion conc _INE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINOUT _ISSETE1)

"))

(th~defproblem _INI_THM (in landau3)
 (conclusion conc _INI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINOUT _ISSETE2)

"))

(th~defproblem _ISI2_THM (in landau3)
 (conclusion conc _ISI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISSETI _ISOUTI)

"))

(th~defproblem _SATZ116_THM (in landau3)
 (conclusion conc _SATZ116)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS)

"))

(th~defproblem _SATZ117_THM (in landau3)
 (conclusion conc _SATZ117)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS)

"))

(th~defproblem _SATZ118_THM (in landau3)
 (conclusion conc _SATZ118)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TRIS)

"))

(th~defproblem _1119_T1_THM (in landau3)
 (conclusion conc _1119_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81B _EC3E23)

"))

(th~defproblem _SATZ119_THM (in landau3)
 (conclusion conc _SATZ119)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A _1119_T1 _TH3)

"))

(th~defproblem _SATZ119A_THM (in landau3)
 (conclusion conc _SATZ119A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ119)

"))

(th~defproblem _1120_T1_THM (in landau3)
 (conclusion conc _1120_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81B _EC3E32)

"))

(th~defproblem _SATZ120_THM (in landau3)
 (conclusion conc _SATZ120)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B _1120_T1 _TH7)

"))

(th~defproblem _SATZ120A_THM (in landau3)
 (conclusion conc _SATZ120A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ82 _SATZ120)

"))

(th~defproblem _III1_T12_THM (in landau3)
 (conclusion conc _III1_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _III1_T13_THM (in landau3)
 (conclusion conc _III1_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T12 _TH3)

"))

(th~defproblem _III1_T14_THM (in landau3)
 (conclusion conc _III1_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T13 _SATZ81F)

"))

(th~defproblem _III1_T15_THM (in landau3)
 (conclusion conc _III1_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP1)

"))

(th~defproblem _III1_T16_THM (in landau3)
 (conclusion conc _III1_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T15 _TH3)

"))

(th~defproblem _III1_T17_THM (in landau3)
 (conclusion conc _III1_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T16 _III1_T14 _ORE1)

"))

(th~defproblem _III1_T18_THM (in landau3)
 (conclusion conc _III1_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T17 _SATZ82)

"))

(th~defproblem _III1_T19_THM (in landau3)
 (conclusion conc _III1_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T18)

"))

(th~defproblem _III1_T20_THM (in landau3)
 (conclusion conc _III1_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _III1_T21_THM (in landau3)
 (conclusion conc _III1_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _III1_T22_THM (in landau3)
 (conclusion conc _III1_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _III1_T23_THM (in landau3)
 (conclusion conc _III1_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T22 _SATZ81G)

"))

(th~defproblem _III1_T24_THM (in landau3)
 (conclusion conc _III1_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ84 _III1_T23 _TH3)

"))

(th~defproblem _III1_T25_THM (in landau3)
 (conclusion conc _III1_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T24 _III1_T21 _TH4)

"))

(th~defproblem _III1_T26_THM (in landau3)
 (conclusion conc _III1_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T25 _ALL_TH1)

"))

(th~defproblem _III1_T27_THM (in landau3)
 (conclusion conc _III1_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T26 _AND_TH1)

"))

(th~defproblem _III1_T28_THM (in landau3)
 (conclusion conc _III1_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T27 _III1_T20 _SOMEAPP)

"))

(th~defproblem _III1_T29_THM (in landau3)
 (conclusion conc _III1_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND_TH2)

"))

(th~defproblem _III1_T30_THM (in landau3)
 (conclusion conc _III1_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T29 _III1_T28 _TH1)

"))

(th~defproblem _III1_T31_THM (in landau3)
 (conclusion conc _III1_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T30 _III1_T11)

"))

(th~defproblem _CUT2_THM (in landau3)
 (conclusion conc _CUT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III1_T31 _III1_T19 _III1_T10 _AND3I)

"))

(th~defproblem _III2_T1_THM (in landau3)
 (conclusion conc _III2_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _III2_T2_THM (in landau3)
 (conclusion conc _III2_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _III2_T3_THM (in landau3)
 (conclusion conc _III2_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III2_T2 _III2_T1)

"))

(th~defproblem _MOREAPP_THM (in landau3)
 (conclusion conc _MOREAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III2_T3 _SOMEAPP)

"))

(th~defproblem _III2_T4_THM (in landau3)
 (conclusion conc _III2_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _III2_T5_THM (in landau3)
 (conclusion conc _III2_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _III2_T6_THM (in landau3)
 (conclusion conc _III2_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III2_T5 _III2_T4)

"))

(th~defproblem _LESSAPP_THM (in landau3)
 (conclusion conc _LESSAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III2_T6 _SOMEAPP)

"))

(th~defproblem _2121_T1_THM (in landau3)
 (conclusion conc _2121_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDI)

"))

(th~defproblem _2121_T2_THM (in landau3)
 (conclusion conc _2121_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2121_T1 _SOMEI)

"))

(th~defproblem _SATZ121_THM (in landau3)
 (conclusion conc _SATZ121)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2121_T2 _MOREAPP)

"))

(th~defproblem _2122_T1_THM (in landau3)
 (conclusion conc _2122_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDI)

"))

(th~defproblem _2122_T2_THM (in landau3)
 (conclusion conc _2122_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2122_T1 _SOMEI)

"))

(th~defproblem _SATZ122_THM (in landau3)
 (conclusion conc _SATZ122)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2122_T2 _LESSAPP)

"))

(th~defproblem _2123_T1_THM (in landau3)
 (conclusion conc _2123_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISSET_TH3)

"))

(th~defproblem _2123_T2_THM (in landau3)
 (conclusion conc _2123_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISE _2123_T1 _TH3)

"))

(th~defproblem _2123_T3_THM (in landau3)
 (conclusion conc _2123_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T2 _MOREAPP)

"))

(th~defproblem _2123_T4_THM (in landau3)
 (conclusion conc _2123_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T3 _EC_TH2)

"))

(th~defproblem _2123_T5_THM (in landau3)
 (conclusion conc _2123_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISSET_TH4)

"))

(th~defproblem _2123_T6_THM (in landau3)
 (conclusion conc _2123_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISE _2123_T5 _TH3)

"))

(th~defproblem _2123_T7_THM (in landau3)
 (conclusion conc _2123_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T6 _LESSAPP)

"))

(th~defproblem _2123_T8_THM (in landau3)
 (conclusion conc _2123_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T7 _EC_TH1)

"))

(th~defproblem _2123_T9_THM (in landau3)
 (conclusion conc _2123_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A)

"))

(th~defproblem _2123_T10_THM (in landau3)
 (conclusion conc _2123_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B)

"))

(th~defproblem _2123_T11_THM (in landau3)
 (conclusion conc _2123_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T10 _SATZ81B _EC3E23 _2123_T9 _MP)

"))

(th~defproblem _2123_T12_THM (in landau3)
 (conclusion conc _2123_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T11 _LESSAPP)

"))

(th~defproblem _2123_T13_THM (in landau3)
 (conclusion conc _2123_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T12 _MOREAPP)

"))

(th~defproblem _2123_T14_THM (in landau3)
 (conclusion conc _2123_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T13)

"))

(th~defproblem _2123_T15_THM (in landau3)
 (conclusion conc _2123_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T14 _EC_TH1)

"))

(th~defproblem _2123_A_THM (in landau3)
 (conclusion conc _2123_A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T8 _2123_T15 _2123_T4 _EC3_TH6)

"))

(th~defproblem _2123_T16_THM (in landau3)
 (conclusion conc _2123_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_OR3I1)

"))

(th~defproblem _2123_T17_THM (in landau3)
 (conclusion conc _2123_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISI _TH3)

"))

(th~defproblem _2123_T18_THM (in landau3)
 (conclusion conc _2123_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T17 _ISSET_TH5)

"))

(th~defproblem _2123_T19_THM (in landau3)
 (conclusion conc _2123_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ121 _2123_T18 _TH8)

"))

(th~defproblem _2123_T20_THM (in landau3)
 (conclusion conc _2123_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T19 _OR3_TH7)

"))

(th~defproblem _2123_B_THM (in landau3)
 (conclusion conc _2123_B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_T20 _2123_T16 _TH1)

"))

(th~defproblem _SATZ123_THM (in landau3)
 (conclusion conc _SATZ123)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2123_A _2123_B _OREC3I)

"))

(th~defproblem _SATZ123A_THM (in landau3)
 (conclusion conc _SATZ123A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123 _OREC3E1)

"))

(th~defproblem _SATZ123B_THM (in landau3)
 (conclusion conc _SATZ123B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123 _OREC3E2)

"))

(th~defproblem _SATZ124_THM (in landau3)
 (conclusion conc _SATZ124)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS _SATZ121 _TH9)

"))

(th~defproblem _SATZ125_THM (in landau3)
 (conclusion conc _SATZ125)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS _SATZ122 _TH9)

"))

(th~defproblem _RP_ISMORE1_THM (in landau3)
 (conclusion conc _RP_ISMORE1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_ISMORE2_THM (in landau3)
 (conclusion conc _RP_ISMORE2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_ISLESS1_THM (in landau3)
 (conclusion conc _RP_ISLESS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_ISLESS2_THM (in landau3)
 (conclusion conc _RP_ISLESS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_ISMOREIS1_THM (in landau3)
 (conclusion conc _RP_ISMOREIS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_ISMOREIS2_THM (in landau3)
 (conclusion conc _RP_ISMOREIS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_ISLESSIS1_THM (in landau3)
 (conclusion conc _RP_ISLESSIS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_ISLESSIS2_THM (in landau3)
 (conclusion conc _RP_ISLESSIS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RP_MOREISI2_THM (in landau3)
 (conclusion conc _RP_MOREISI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _RP_LESSISI2_THM (in landau3)
 (conclusion conc _RP_LESSISI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _RP_MOREISI1_THM (in landau3)
 (conclusion conc _RP_MOREISI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _RP_LESSISI1_THM (in landau3)
 (conclusion conc _RP_LESSISI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _RP_ISMORE12_THM (in landau3)
 (conclusion conc _RP_ISMORE12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISMORE1 _RP_ISMORE2)

"))

(th~defproblem _RP_ISLESS12_THM (in landau3)
 (conclusion conc _RP_ISLESS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISLESS1 _RP_ISLESS2)

"))

(th~defproblem _RP_ISMOREIS12_THM (in landau3)
 (conclusion conc _RP_ISMOREIS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISMOREIS1 _RP_ISMOREIS2)

"))

(th~defproblem _RP_ISLESSIS12_THM (in landau3)
 (conclusion conc _RP_ISLESSIS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISLESSIS1 _RP_ISLESSIS2)

"))

(th~defproblem _SATZ123C_THM (in landau3)
 (conclusion conc _SATZ123C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMOR _SATZ123B _EC3_TH7)

"))

(th~defproblem _SATZ123D_THM (in landau3)
 (conclusion conc _SATZ123D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123B _EC3_TH9)

"))

(th~defproblem _SATZ123E_THM (in landau3)
 (conclusion conc _SATZ123E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123A _OR3_TH2)

"))

(th~defproblem _SATZ123F_THM (in landau3)
 (conclusion conc _SATZ123F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123A _OR3_TH3 _COMOR)

"))

(th~defproblem _SATZ123G_THM (in landau3)
 (conclusion conc _SATZ123G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123B _EC3E21 _EC3E23 _OR_TH3)

"))

(th~defproblem _SATZ123H_THM (in landau3)
 (conclusion conc _SATZ123H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123B _EC3E31 _EC3E32 _OR_TH3)

"))

(th~defproblem _SATZ123J_THM (in landau3)
 (conclusion conc _SATZ123J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_OR_TH4 _OR_TH5 _SATZ123A _OR3E3)

"))

(th~defproblem _SATZ123K_THM (in landau3)
 (conclusion conc _SATZ123K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_OR_TH5 _OR_TH4 _SATZ123A _OR3E2)

"))

(th~defproblem _2126_T1_THM (in landau3)
 (conclusion conc _2126_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A)

"))

(th~defproblem _2126_T2_THM (in landau3)
 (conclusion conc _2126_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2126_T1 _SATZ119A)

"))

(th~defproblem _2126_T3_THM (in landau3)
 (conclusion conc _2126_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2126_T2 _ANDI)

"))

(th~defproblem _2126_T4_THM (in landau3)
 (conclusion conc _2126_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2126_T3 _SOMEI)

"))

(th~defproblem _2126_T5_THM (in landau3)
 (conclusion conc _2126_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2126_T4 _LESSAPP)

"))

(th~defproblem _SATZ126_THM (in landau3)
 (conclusion conc _SATZ126)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2126_T5 _LESSAPP)

"))

(th~defproblem _RP_TRLESS_THM (in landau3)
 (conclusion conc _RP_TRLESS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ126)

"))

(th~defproblem _RP_TRMORE_THM (in landau3)
 (conclusion conc _RP_TRMORE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ121 _SATZ126 _SATZ122)

"))

(th~defproblem _SATZ127A_THM (in landau3)
 (conclusion conc _SATZ127A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS _RP_ISLESS1 _RP_TRLESS _ORAPP)

"))

(th~defproblem _SATZ127B_THM (in landau3)
 (conclusion conc _SATZ127B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISLESS2 _RP_TRLESS _ORAPP)

"))

(th~defproblem _SATZ127C_THM (in landau3)
 (conclusion conc _SATZ127C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ124 _SATZ121 _SATZ127B _SATZ122)

"))

(th~defproblem _SATZ127D_THM (in landau3)
 (conclusion conc _SATZ127D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ121 _SATZ124 _SATZ127A _SATZ122)

"))

(th~defproblem _2128_T1_THM (in landau3)
 (conclusion conc _2128_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TRIS _RP_LESSISI2)

"))

(th~defproblem _2128_T2_THM (in landau3)
 (conclusion conc _2128_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ127A _RP_LESSISI1)

"))

(th~defproblem _2128_T3_THM (in landau3)
 (conclusion conc _2128_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2128_T1 _2128_T2 _ORAPP)

"))

(th~defproblem _2128_T4_THM (in landau3)
 (conclusion conc _2128_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ127B _RP_LESSISI1)

"))

(th~defproblem _SATZ128_THM (in landau3)
 (conclusion conc _SATZ128)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_2128_T3 _2128_T4 _ORAPP)

"))

(th~defproblem _RP_TRLESSIS_THM (in landau3)
 (conclusion conc _RP_TRLESSIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ128)

"))

(th~defproblem _RP_TRMOREIS_THM (in landau3)
 (conclusion conc _RP_TRMOREIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ124 _SATZ128 _SATZ125)

"))

(th~defproblem _III3_T1_THM (in landau3)
 (conclusion conc _III3_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3I)

"))

(th~defproblem _III3_T2_THM (in landau3)
 (conclusion conc _III3_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T1 _SOMEI)

"))

(th~defproblem _III3_T3_THM (in landau3)
 (conclusion conc _III3_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T2 _SOMEI)

"))

(th~defproblem _SUM1_THM (in landau3)
 (conclusion conc _SUM1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T3 _ESTII)

"))

(th~defproblem _III3_T4_THM (in landau3)
 (conclusion conc _III3_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _III3_T5_THM (in landau3)
 (conclusion conc _III3_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E1)

"))

(th~defproblem _III3_T6_THM (in landau3)
 (conclusion conc _III3_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E2)

"))

(th~defproblem _III3_T7_THM (in landau3)
 (conclusion conc _III3_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E3)

"))

(th~defproblem _III3_T8_THM (in landau3)
 (conclusion conc _III3_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T7 _III3_T6 _III3_T5)

"))

(th~defproblem _III3_T9_THM (in landau3)
 (conclusion conc _III3_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T8 _SOMEAPP)

"))

(th~defproblem _SUMAPP_THM (in landau3)
 (conclusion conc _SUMAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T9 _III3_T4 _SOMEAPP)

"))

(th~defproblem _3129_T1_THM (in landau3)
 (conclusion conc _3129_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A)

"))

(th~defproblem _3129_T2_THM (in landau3)
 (conclusion conc _3129_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A)

"))

(th~defproblem _3129_T3_THM (in landau3)
 (conclusion conc _3129_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T2 _3129_T1 _SATZ98A _SYMIS _RT_ISLESS1)

"))

(th~defproblem _3129_T4_THM (in landau3)
 (conclusion conc _3129_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T3 _SATZ81B _EC3E31)

"))

(th~defproblem _3129_T5_THM (in landau3)
 (conclusion conc _3129_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T4 _SUMAPP)

"))

(th~defproblem _SATZ129A_THM (in landau3)
 (conclusion conc _SATZ129A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T5 _REFIS _WELI _TH3)

"))

(th~defproblem _3129_T6_THM (in landau3)
 (conclusion conc _3129_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISLESS2)

"))

(th~defproblem _3129_T7_THM (in landau3)
 (conclusion conc _3129_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T6 _EXAMPLE1D _SATZ110F _RT_ISLESS12)

"))

(th~defproblem _3129_T8_THM (in landau3)
 (conclusion conc _3129_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T7 _SATZ106C)

"))

(th~defproblem _3129_T9_THM (in landau3)
 (conclusion conc _3129_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T8 _SATZ105F _EXAMPLE1A _RT_ISLESS2)

"))

(th~defproblem _3129_T10_THM (in landau3)
 (conclusion conc _3129_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T8 _SATZ105F _EXAMPLE1A _RT_ISLESS2)

"))

(th~defproblem _3129_T11_THM (in landau3)
 (conclusion conc _3129_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T9 _SATZ120)

"))

(th~defproblem _3129_T12_THM (in landau3)
 (conclusion conc _3129_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T10 _SATZ120)

"))

(th~defproblem _3129_T13_THM (in landau3)
 (conclusion conc _3129_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110C _RT_DISTPT1 _TRIS)

"))

(th~defproblem _3129_T14_THM (in landau3)
 (conclusion conc _3129_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T13 _SYMIS)

"))

(th~defproblem _3129_T15_THM (in landau3)
 (conclusion conc _3129_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T14 _3129_T12 _3129_T11 _SUM1)

"))

(th~defproblem _3129_T16_THM (in landau3)
 (conclusion conc _3129_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T15 _SUMAPP)

"))

(th~defproblem _3129_T17_THM (in landau3)
 (conclusion conc _3129_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _SUM1)

"))

(th~defproblem _3129_T18_THM (in landau3)
 (conclusion conc _3129_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ96A)

"))

(th~defproblem _3129_T19_THM (in landau3)
 (conclusion conc _3129_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T18 _SYMIS _RT_ISMORE2)

"))

(th~defproblem _3129_T20_THM (in landau3)
 (conclusion conc _3129_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T19 _3129_T17 _ANDI)

"))

(th~defproblem _3129_T21_THM (in landau3)
 (conclusion conc _3129_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T20 _SOMEI)

"))

(th~defproblem _3129_T22_THM (in landau3)
 (conclusion conc _3129_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T21 _CUTAPP3)

"))

(th~defproblem _3129_T23_THM (in landau3)
 (conclusion conc _3129_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T22 _SUMAPP)

"))

(th~defproblem _3129_T24_THM (in landau3)
 (conclusion conc _3129_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T23 _3129_T16 _SATZ129A _REFIS _SUM1 _CUT2)

"))

(th~defproblem _3129_T25_THM (in landau3)
 (conclusion conc _3129_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T24 _CUTAPP1B)

"))

(th~defproblem _3129_T26_THM (in landau3)
 (conclusion conc _3129_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T25 _CUTAPP1B)

"))

(th~defproblem _3129_T27_THM (in landau3)
 (conclusion conc _3129_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T26 _CUTAPP1A)

"))

(th~defproblem _SATZ129_THM (in landau3)
 (conclusion conc _SATZ129)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3129_T27 _CUTAPP1A)

"))

(th~defproblem _LRTPL_THM (in landau3)
 (conclusion conc _LRTPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SUM1 _SATZ129 _INE)

"))

(th~defproblem _III3_T10_THM (in landau3)
 (conclusion conc _III3_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ129A _ISP1)

"))

(th~defproblem _URTPL_THM (in landau3)
 (conclusion conc _URTPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ129 _INI _III3_T10 _TH3)

"))

(th~defproblem _III3_T11_THM (in landau3)
 (conclusion conc _III3_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ129 _INI)

"))

(th~defproblem _PLAPP_THM (in landau3)
 (conclusion conc _PLAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T11 _SUMAPP)

"))

(th~defproblem _RP_ISPL1_THM (in landau3)
 (conclusion conc _RP_ISPL1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RP_ISPL2_THM (in landau3)
 (conclusion conc _RP_ISPL2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RP_ISPL12_THM (in landau3)
 (conclusion conc _RP_ISPL12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISPL2 _RP_ISPL1 _TRIS)

"))

(th~defproblem _3130_T1_THM (in landau3)
 (conclusion conc _3130_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_COMPL _TRIS)

"))

(th~defproblem _3130_T2_THM (in landau3)
 (conclusion conc _3130_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3130_T1 _LRTPL)

"))

(th~defproblem _3130_T3_THM (in landau3)
 (conclusion conc _3130_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3130_T2 _PLAPP)

"))

(th~defproblem _SATZ130_THM (in landau3)
 (conclusion conc _SATZ130)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3130_T3 _ISI1)

"))

(th~defproblem _RP_COMPL_THM (in landau3)
 (conclusion conc _RP_COMPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ130)

"))

(th~defproblem _3131_T1_THM (in landau3)
 (conclusion conc _3131_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ASSPL1 _RT_ISPL1 _TR3IS)

"))

(th~defproblem _3131_T2_THM (in landau3)
 (conclusion conc _3131_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _LRTPL)

"))

(th~defproblem _3131_T3_THM (in landau3)
 (conclusion conc _3131_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3131_T1 _3131_T2 _LRTPL)

"))

(th~defproblem _3131_T4_THM (in landau3)
 (conclusion conc _3131_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3131_T3 _PLAPP)

"))

(th~defproblem _3131_T5_THM (in landau3)
 (conclusion conc _3131_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3131_T4 _PLAPP)

"))

(th~defproblem _3131_T6_THM (in landau3)
 (conclusion conc _3131_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ASSPL2 _RT_ISPL2 _TR3IS)

"))

(th~defproblem _3131_T7_THM (in landau3)
 (conclusion conc _3131_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _LRTPL)

"))

(th~defproblem _3131_T8_THM (in landau3)
 (conclusion conc _3131_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3131_T6 _3131_T7 _LRTPL)

"))

(th~defproblem _3131_T9_THM (in landau3)
 (conclusion conc _3131_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3131_T8 _PLAPP)

"))

(th~defproblem _3131_T10_THM (in landau3)
 (conclusion conc _3131_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3131_T9 _PLAPP)

"))

(th~defproblem _SATZ131_THM (in landau3)
 (conclusion conc _SATZ131)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3131_T10 _3131_T5 _ISI1)

"))

(th~defproblem _RP_ASSPL1_THM (in landau3)
 (conclusion conc _RP_ASSPL1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ131)

"))

(th~defproblem _RP_ASSPL2_THM (in landau3)
 (conclusion conc _RP_ASSPL2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ131 _SYMIS)

"))

(th~defproblem _3132_T1_THM (in landau3)
 (conclusion conc _3132_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2 _ANDE1 _CUTAPP2B)

"))

(th~defproblem _3132_T2_THM (in landau3)
 (conclusion conc _3132_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B)

"))

(th~defproblem _3132_T3_THM (in landau3)
 (conclusion conc _3132_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T2 _SATZ96D)

"))

(th~defproblem _3132_T4_THM (in landau3)
 (conclusion conc _3132_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T3 _3132_T2 _SATZ101C _RT_ISMORE2)

"))

(th~defproblem _3132_T5_THM (in landau3)
 (conclusion conc _3132_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T4 _SATZ119 _3132_T2)

"))

(th~defproblem _3132_T6_THM (in landau3)
 (conclusion conc _3132_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T5 _SOMEI _3132_T2)

"))

(th~defproblem _3132_T7_THM (in landau3)
 (conclusion conc _3132_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T6 _3132_T2 _SATZ115 _SOMEAPP)

"))

(th~defproblem _3132_T8_THM (in landau3)
 (conclusion conc _3132_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _3132_T9_THM (in landau3)
 (conclusion conc _3132_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _3132_T10_THM (in landau3)
 (conclusion conc _3132_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1A _RT_COMTS _ISNERT _RT_ISTS1 _TR3IS)

"))

(th~defproblem _3132_T11_THM (in landau3)
 (conclusion conc _3132_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T10 _3132_T9 _ISP)

"))

(th~defproblem _3132_T12_THM (in landau3)
 (conclusion conc _3132_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T11 _ANDI)

"))

(th~defproblem _3132_T13_THM (in landau3)
 (conclusion conc _3132_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _3132_T1 _SATZ101G _SYMIS)

"))

(th~defproblem _3132_T14_THM (in landau3)
 (conclusion conc _3132_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T13 _3132_T12 _ANDI)

"))

(th~defproblem _3132_T15_THM (in landau3)
 (conclusion conc _3132_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T14 _SOMEI)

"))

(th~defproblem _3132_T16_THM (in landau3)
 (conclusion conc _3132_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T15 _SOMEI)

"))

(th~defproblem _3132_T17_THM (in landau3)
 (conclusion conc _3132_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ111D _INCLASS _MOREI)

"))

(th~defproblem _3132_T18_THM (in landau3)
 (conclusion conc _3132_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T17 _NATRTI _SATZ112G)

"))

(th~defproblem _3132_T19_THM (in landau3)
 (conclusion conc _3132_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ94A _3132_T17 _SATZ101E _RT_ISLESS2)

"))

(th~defproblem _3132_T20_THM (in landau3)
 (conclusion conc _3132_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T19 _INCLASS _3132_T18 _INCLASSN _LESSE)

"))

(th~defproblem _3132_T21_THM (in landau3)
 (conclusion conc _3132_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T20 _SATZ111C)

"))

(th~defproblem _3132_T22_THM (in landau3)
 (conclusion conc _3132_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ14 _3132_T21 _SATZ10H _TH3)

"))

(th~defproblem _3132_T23_THM (in landau3)
 (conclusion conc _3132_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T8 _3132_T22 _TH3 _ET)

"))

(th~defproblem _3132_T24_THM (in landau3)
 (conclusion conc _3132_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T18 _ISRTN1 _3132_T23 _ISP1)

"))

(th~defproblem _3132_T25_THM (in landau3)
 (conclusion conc _3132_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T9 _3132_T24 _ANDI)

"))

(th~defproblem _3132_T26_THM (in landau3)
 (conclusion conc _3132_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T17 _SATZ101E _RT_ISTS1 _RT_DISTPT1 _EXAMPLE1D _RT_ISPL2 _TR3IS)

"))

(th~defproblem _3132_T27_THM (in landau3)
 (conclusion conc _3132_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T26 _RT_ISPL2 _RT_ASSPL1 _TRIS)

"))

(th~defproblem _3132_T28_THM (in landau3)
 (conclusion conc _3132_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T27 _3132_T1 _SATZ101G _SYMIS)

"))

(th~defproblem _3132_T29_THM (in landau3)
 (conclusion conc _3132_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T28 _3132_T25 _ANDI)

"))

(th~defproblem _3132_T30_THM (in landau3)
 (conclusion conc _3132_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T29 _SOMEI)

"))

(th~defproblem _3132_T31_THM (in landau3)
 (conclusion conc _3132_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T30 _SOMEI)

"))

(th~defproblem _3132_T32_THM (in landau3)
 (conclusion conc _3132_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T16 _3132_T31 _SATZ24 _ORAPP)

"))

(th~defproblem _3132_T34_THM (in landau3)
 (conclusion conc _3132_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T32 _3132_T7 _SATZ27 _SOMEAPP)

"))

(th~defproblem _3132_T35_THM (in landau3)
 (conclusion conc _3132_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T34 _CUTAPP1B)

"))

(th~defproblem _SATZ132_THM (in landau3)
 (conclusion conc _SATZ132)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T35 _CUTAPP1A _ANDE2 _ANDE1 _CUTAPP2B)

"))

(th~defproblem _3132_T36_THM (in landau3)
 (conclusion conc _3132_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1 _CUTAPP2B)

"))

(th~defproblem _3132_T37_THM (in landau3)
 (conclusion conc _3132_T37)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_R_ANDE2 _CUTAPP2B _3132_T36)

"))

(th~defproblem _3132_T38_THM (in landau3)
 (conclusion conc _3132_T38)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T36 _ANDE1 _CUTAPP2B)

"))

(th~defproblem _3132_T39_THM (in landau3)
 (conclusion conc _3132_T39)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T36 _ANDE2 _CUTAPP2B)

"))

(th~defproblem _3132_T40_THM (in landau3)
 (conclusion conc _3132_T40)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T39 _3132_T38 _CUTAPP2B _SATZ101C _3132_T36 _3132_T1 _SATZ101G)

"))

(th~defproblem _3132_T41_THM (in landau3)
 (conclusion conc _3132_T41)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T37 _3132_T40 _3132_T36 _3132_T1 _3132_T39 _3132_T38 _CUTAPP2B _TRIS)

"))

(th~defproblem _3132_T42_THM (in landau3)
 (conclusion conc _3132_T42)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T41 _3132_T39 _3132_T38 _CUTAPP2B)

"))

(th~defproblem _3132_T43_THM (in landau3)
 (conclusion conc _3132_T43)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T42 _SOMEAPP _CUTAPP2B)

"))

(th~defproblem _SATZ132APP_THM (in landau3)
 (conclusion conc _SATZ132APP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3132_T43 _SATZ132 _SOMEAPP _CUTAPP2B)

"))

(th~defproblem _3133_T1_THM (in landau3)
 (conclusion conc _3133_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B _RT_ISPL2 _SATZ101D _TRIS)

"))

(th~defproblem _3133_T2_THM (in landau3)
 (conclusion conc _3133_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3133_T1 _LRTPL _CUTAPP2B)

"))

(th~defproblem _3133_T3_THM (in landau3)
 (conclusion conc _3133_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3133_T2 _ANDI _CUTAPP2B)

"))

(th~defproblem _3133_T4_THM (in landau3)
 (conclusion conc _3133_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3133_T3 _SOMEI _CUTAPP2B)

"))

(th~defproblem _3133_T5_THM (in landau3)
 (conclusion conc _3133_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3133_T4 _CUTAPP2B _SATZ132APP)

"))

(th~defproblem _SATZ133_THM (in landau3)
 (conclusion conc _SATZ133)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3133_T5 _CUTAPP1A)

"))

(th~defproblem _SATZ133A_THM (in landau3)
 (conclusion conc _SATZ133A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ133 _SATZ121)

"))

(th~defproblem _3134_T1_THM (in landau3)
 (conclusion conc _3134_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ119A)

"))

(th~defproblem _3134_T2_THM (in landau3)
 (conclusion conc _3134_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83)

"))

(th~defproblem _3134_T3_THM (in landau3)
 (conclusion conc _3134_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T2 _CUTAPP2B _RT_ISPL1 _SATZ101F _TRIS)

"))

(th~defproblem _3134_T4_THM (in landau3)
 (conclusion conc _3134_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T2 _SATZ101C _RT_ISPL1 _RT_ASSPL2 _3134_T3 _RT_ISPL2 _TR3IS _CUTAPP2B)

"))

(th~defproblem _3134_T5_THM (in landau3)
 (conclusion conc _3134_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T4 _LRTPL _3134_T2 _CUTAPP2B)

"))

(th~defproblem _3134_T6_THM (in landau3)
 (conclusion conc _3134_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _URTPL _3134_T2 _CUTAPP2B)

"))

(th~defproblem _3134_T7_THM (in landau3)
 (conclusion conc _3134_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T6 _3134_T5 _ANDI _3134_T2 _CUTAPP2B)

"))

(th~defproblem _3134_T8_THM (in landau3)
 (conclusion conc _3134_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T7 _SOMEI _3134_T2 _CUTAPP2B)

"))

(th~defproblem _3134_T9_THM (in landau3)
 (conclusion conc _3134_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T8 _3134_T2 _CUTAPP2B _SATZ132APP)

"))

(th~defproblem _3134_T10_THM (in landau3)
 (conclusion conc _3134_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T9 _CUTAPP3)

"))

(th~defproblem _SATZ134_THM (in landau3)
 (conclusion conc _SATZ134)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T10 _MOREAPP)

"))

(th~defproblem _SATZ135A_THM (in landau3)
 (conclusion conc _SATZ135A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ134)

"))

(th~defproblem _SATZ135B_THM (in landau3)
 (conclusion conc _SATZ135B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISPL1)

"))

(th~defproblem _SATZ135C_THM (in landau3)
 (conclusion conc _SATZ135C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122 _SATZ134 _SATZ121)

"))

(th~defproblem _SATZ135D_THM (in landau3)
 (conclusion conc _SATZ135D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135A _RP_COMPL _RP_ISMORE12)

"))

(th~defproblem _SATZ135E_THM (in landau3)
 (conclusion conc _SATZ135E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISPL2)

"))

(th~defproblem _SATZ135F_THM (in landau3)
 (conclusion conc _SATZ135F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135C _RP_COMPL _RP_ISLESS12)

"))

(th~defproblem _SATZ135G_THM (in landau3)
 (conclusion conc _SATZ135G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135D _RP_ISPL1 _RP_ISMORE2)

"))

(th~defproblem _SATZ135H_THM (in landau3)
 (conclusion conc _SATZ135H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135G _RP_COMPL _RP_ISMORE12)

"))

(th~defproblem _SATZ135J_THM (in landau3)
 (conclusion conc _SATZ135J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135F _RP_ISPL1 _RP_ISLESS2)

"))

(th~defproblem _SATZ135K_THM (in landau3)
 (conclusion conc _SATZ135K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135J _RP_COMPL _RP_ISLESS12)

"))

(th~defproblem _3136_T1_THM (in landau3)
 (conclusion conc _3136_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123A)

"))

(th~defproblem _3136_T2_THM (in landau3)
 (conclusion conc _3136_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123B)

"))

(th~defproblem _SATZ136A_THM (in landau3)
 (conclusion conc _SATZ136A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135C _SATZ135A _SATZ135B _3136_T2 _3136_T1 _EC3_TH11)

"))

(th~defproblem _SATZ136B_THM (in landau3)
 (conclusion conc _SATZ136B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135C _SATZ135A _SATZ135B _3136_T2 _3136_T1 _EC3_TH10)

"))

(th~defproblem _SATZ136C_THM (in landau3)
 (conclusion conc _SATZ136C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135C _SATZ135A _SATZ135B _3136_T2 _3136_T1 _EC3_TH12)

"))

(th~defproblem _SATZ136D_THM (in landau3)
 (conclusion conc _SATZ136D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMPL _RP_ISMORE12 _SATZ136A)

"))

(th~defproblem _SATZ136E_THM (in landau3)
 (conclusion conc _SATZ136E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMPL _TR3IS _SATZ136B)

"))

(th~defproblem _SATZ136F_THM (in landau3)
 (conclusion conc _SATZ136F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMPL _RP_ISLESS12 _SATZ136C)

"))

(th~defproblem _3137_T1_THM (in landau3)
 (conclusion conc _3137_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ134)

"))

(th~defproblem _3137_T2_THM (in landau3)
 (conclusion conc _3137_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ134 _RP_COMPL _RP_ISMORE12)

"))

(th~defproblem _SATZ137_THM (in landau3)
 (conclusion conc _SATZ137)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3137_T2 _3137_T1 _RP_TRMORE)

"))

(th~defproblem _SATZ137A_THM (in landau3)
 (conclusion conc _SATZ137A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122 _SATZ137 _SATZ121)

"))

(th~defproblem _SATZ138A_THM (in landau3)
 (conclusion conc _SATZ138A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135G _SATZ137 _ORAPP)

"))

(th~defproblem _SATZ138B_THM (in landau3)
 (conclusion conc _SATZ138B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135H _SATZ137 _ORAPP)

"))

(th~defproblem _SATZ138C_THM (in landau3)
 (conclusion conc _SATZ138C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122 _SATZ125 _SATZ138A _SATZ121)

"))

(th~defproblem _SATZ138D_THM (in landau3)
 (conclusion conc _SATZ138D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ125 _SATZ122 _SATZ138B _SATZ121)

"))

(th~defproblem _3139_T1_THM (in landau3)
 (conclusion conc _3139_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISPL12 _RP_MOREISI2)

"))

(th~defproblem _3139_T2_THM (in landau3)
 (conclusion conc _3139_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ138A _RP_MOREISI1)

"))

(th~defproblem _3139_T3_THM (in landau3)
 (conclusion conc _3139_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3139_T1 _3139_T2 _ORAPP)

"))

(th~defproblem _3139_T4_THM (in landau3)
 (conclusion conc _3139_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ138B _RP_MOREISI1)

"))

(th~defproblem _SATZ139_THM (in landau3)
 (conclusion conc _SATZ139)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3139_T3 _3139_T4 _ORAPP)

"))

(th~defproblem _SATZ139A_THM (in landau3)
 (conclusion conc _SATZ139A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ125 _SATZ139 _SATZ124)

"))

(th~defproblem _3140_T1_THM (in landau3)
 (conclusion conc _3140_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ133 _RP_ISMORE1)

"))

(th~defproblem _3140_T2_THM (in landau3)
 (conclusion conc _3140_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T1 _SATZ123D _TH3)

"))

(th~defproblem _VORBEMERKUNG140_THM (in landau3)
 (conclusion conc _VORBEMERKUNG140)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T2 _SOME_TH5)

"))

(th~defproblem _3140_T3_THM (in landau3)
 (conclusion conc _3140_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135D)

"))

(th~defproblem _3140_T4_THM (in landau3)
 (conclusion conc _3140_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T3 _SATZ123B _EC3E21)

"))

(th~defproblem _3140_T5_THM (in landau3)
 (conclusion conc _3140_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ135F)

"))

(th~defproblem _3140_T6_THM (in landau3)
 (conclusion conc _3140_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T5 _SATZ123B _EC3E31)

"))

(th~defproblem _3140_T7_THM (in landau3)
 (conclusion conc _3140_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123A _OR3_TH1)

"))

(th~defproblem _3140_T8_THM (in landau3)
 (conclusion conc _3140_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T6 _3140_T4 _3140_T7 _ORAPP)

"))

(th~defproblem _SATZ140B_THM (in landau3)
 (conclusion conc _SATZ140B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T8 _TRIS2 _WELI _TH7)

"))

(th~defproblem _T11A_THM (in landau3)
 (conclusion conc _T11A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101C _SATZ101G _TRIS)

"))

(th~defproblem _III3_T12_THM (in landau3)
 (conclusion conc _III3_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T11A _ANDI)

"))

(th~defproblem _III3_T13_THM (in landau3)
 (conclusion conc _III3_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T12 _AND3I)

"))

(th~defproblem _III3_T14_THM (in landau3)
 (conclusion conc _III3_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T13 _SOMEI)

"))

(th~defproblem _III3_T15_THM (in landau3)
 (conclusion conc _III3_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T14 _SOMEI)

"))

(th~defproblem _DIFF1_THM (in landau3)
 (conclusion conc _DIFF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T15 _ESTII)

"))

(th~defproblem _III3_T16_THM (in landau3)
 (conclusion conc _III3_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _III3_T17_THM (in landau3)
 (conclusion conc _III3_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E1)

"))

(th~defproblem _III3_T18_THM (in landau3)
 (conclusion conc _III3_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E2)

"))

(th~defproblem _III3_T19_THM (in landau3)
 (conclusion conc _III3_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E3)

"))

(th~defproblem _III3_T20_THM (in landau3)
 (conclusion conc _III3_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T19 _ANDE1)

"))

(th~defproblem _III3_T21_THM (in landau3)
 (conclusion conc _III3_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T19 _R_ANDE2 _III3_T20)

"))

(th~defproblem _III3_T22_THM (in landau3)
 (conclusion conc _III3_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T21 _III3_T20 _III3_T18 _III3_T17)

"))

(th~defproblem _III3_T23_THM (in landau3)
 (conclusion conc _III3_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T22 _SOMEAPP)

"))

(th~defproblem _DIFFAPP_THM (in landau3)
 (conclusion conc _DIFFAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III3_T23 _III3_T16 _SOMEAPP)

"))

(th~defproblem _3140_T9_THM (in landau3)
 (conclusion conc _3140_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3134_T2)

"))

(th~defproblem _3140_T10_THM (in landau3)
 (conclusion conc _3140_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T9 _REFIS _DIFF1)

"))

(th~defproblem _3140_T11_THM (in landau3)
 (conclusion conc _3140_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ94A _SATZ101E _SYMIS _RT_ISLESS12)

"))

(th~defproblem _3140_T12_THM (in landau3)
 (conclusion conc _3140_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A _3140_T11 _RT_TRLESS)

"))

(th~defproblem _3140_T13_THM (in landau3)
 (conclusion conc _3140_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T12 _SATZ81B _EC3E31)

"))

(th~defproblem _3140_T14_THM (in landau3)
 (conclusion conc _3140_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T13 _DIFFAPP)

"))

(th~defproblem _3140_T15_THM (in landau3)
 (conclusion conc _3140_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T14 _REFIS _WELI _TH3)

"))

(th~defproblem _3140_T16_THM (in landau3)
 (conclusion conc _3140_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101E _RT_ISPL1 _TRIS)

"))

(th~defproblem _3140_T17_THM (in landau3)
 (conclusion conc _3140_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ96C _3140_T16 _RT_ISLESS2)

"))

(th~defproblem _3140_T18_THM (in landau3)
 (conclusion conc _3140_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T17 _SATZ120)

"))

(th~defproblem _3140_T19_THM (in landau3)
 (conclusion conc _3140_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ94 _RT_COMPL _RT_ISMORE1)

"))

(th~defproblem _3140_T20_THM (in landau3)
 (conclusion conc _3140_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_COMPL _3140_T19 _SATZ101G)

"))

(th~defproblem _3140_T21_THM (in landau3)
 (conclusion conc _3140_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T20 _3140_T19 _3140_T18 _DIFF1)

"))

(th~defproblem _3140_T22_THM (in landau3)
 (conclusion conc _3140_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T21 _DIFFAPP)

"))

(th~defproblem _3140_T23_THM (in landau3)
 (conclusion conc _3140_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83)

"))

(th~defproblem _3140_T24_THM (in landau3)
 (conclusion conc _3140_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T23 _RT_TRMORE)

"))

(th~defproblem _3140_T25_THM (in landau3)
 (conclusion conc _3140_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T23 _SATZ101F _3140_T24 _RT_ISMORE12)

"))

(th~defproblem _3140_T26_THM (in landau3)
 (conclusion conc _3140_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T25 _3140_T24 _SATZ97A)

"))

(th~defproblem _3140_T27_THM (in landau3)
 (conclusion conc _3140_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T26 _SYMIS _3140_T24 _RT_ISMORE2)

"))

(th~defproblem _3140_T28_THM (in landau3)
 (conclusion conc _3140_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T24 _REFIS _DIFF1)

"))

(th~defproblem _3140_T29_THM (in landau3)
 (conclusion conc _3140_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T27 _3140_T28 _3140_T24 _ANDI)

"))

(th~defproblem _3140_T30_THM (in landau3)
 (conclusion conc _3140_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T29 _3140_T24 _SOMEI)

"))

(th~defproblem _3140_T31_THM (in landau3)
 (conclusion conc _3140_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T30 _CUTAPP3)

"))

(th~defproblem _3140_T32_THM (in landau3)
 (conclusion conc _3140_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T31 _DIFFAPP)

"))

(th~defproblem _3140_T33_THM (in landau3)
 (conclusion conc _3140_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T32 _3140_T22 _3140_T15 _3140_T10 _3140_T9 _CUT2)

"))

(th~defproblem _3140_T34_THM (in landau3)
 (conclusion conc _3140_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T33 _CUTAPP1B)

"))

(th~defproblem _3140_T35_THM (in landau3)
 (conclusion conc _3140_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T34 _CUTAPP3)

"))

(th~defproblem _SATZ140H_THM (in landau3)
 (conclusion conc _SATZ140H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T35 _MOREAPP)

"))

(th~defproblem _3140_T36_THM (in landau3)
 (conclusion conc _3140_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B)

"))

(th~defproblem _3140_T37_THM (in landau3)
 (conclusion conc _3140_T37)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101E _3140_T36 _SATZ101C _RT_ISPL2 _RT_ASSPL1 _TR3IS)

"))

(th~defproblem _3140_T38_THM (in landau3)
 (conclusion conc _3140_T38)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T36 _SATZ94A _3140_T37 _RT_ISLESS2)

"))

(th~defproblem _3140_T39_THM (in landau3)
 (conclusion conc _3140_T39)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISPL1 _RT_COMPL _TR3IS)

"))

(th~defproblem _3140_T40_THM (in landau3)
 (conclusion conc _3140_T40)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T38 _3140_T39 _SYMIS _RT_ISLESS1)

"))

(th~defproblem _3140_T41_THM (in landau3)
 (conclusion conc _3140_T41)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T40 _SATZ120)

"))

(th~defproblem _3140_T42_THM (in landau3)
 (conclusion conc _3140_T42)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T41 _SATZ140H _INI _DIFFAPP)

"))

(th~defproblem _3140_A_THM (in landau3)
 (conclusion conc _3140_A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T42 _PLAPP)

"))

(th~defproblem _3140_T43_THM (in landau3)
 (conclusion conc _3140_T43)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83)

"))

(th~defproblem _3140_T44_THM (in landau3)
 (conclusion conc _3140_T44)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B)

"))

(th~defproblem _3140_T45_THM (in landau3)
 (conclusion conc _3140_T45)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B _3140_T43 _3140_T44)

"))

(th~defproblem _3140_T46_THM (in landau3)
 (conclusion conc _3140_T46)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T43 _3140_T44 _RT_ISPL1 _SATZ101F _TRIS)

"))

(th~defproblem _3140_T47_THM (in landau3)
 (conclusion conc _3140_T47)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T43 _SATZ101E _3140_T45 _SATZ101C _RT_ISPL2 _RT_ASSPL1 _3140_T46 _RT_ISPL1 _TR4IS _3140_T44)

"))

(th~defproblem _3140_T48_THM (in landau3)
 (conclusion conc _3140_T48)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T45 _SATZ94 _3140_T47 _RT_ISMORE1 _3140_T43 _3140_T44)

"))

(th~defproblem _T49_THM (in landau3)
 (conclusion conc _T49)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T47 _3140_T48 _3140_T45 _SATZ101G _3140_T43 _3140_T44)

"))

(th~defproblem _T50_THM (in landau3)
 (conclusion conc _T50)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T49 _3140_T48 _3140_T45 _RT_ISPL1 _SATZ101F _TRIS _3140_T43 _3140_T44)

"))

(th~defproblem _T51_THM (in landau3)
 (conclusion conc _T51)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T48 _REFIS _DIFF1 _SATZ140H _INE _3140_T43 _3140_T44)

"))

(th~defproblem _T52_THM (in landau3)
 (conclusion conc _T52)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_T48 _RT_COMPL _T50 _TRIS _T51 _LRTPL _3140_T43 _3140_T44)

"))

(th~defproblem _T53_THM (in landau3)
 (conclusion conc _T53)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T52 _3140_T43 _CUTAPP2B _SATZ132APP)

"))

(th~defproblem _T54_THM (in landau3)
 (conclusion conc _T54)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T53 _CUTAPP3)

"))

(th~defproblem _T55_THM (in landau3)
 (conclusion conc _T55)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T54)

"))

(th~defproblem _T56_THM (in landau3)
 (conclusion conc _T56)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A _T55 _SATZ120)

"))

(th~defproblem _T57_THM (in landau3)
 (conclusion conc _T57)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T56 _MOREAPP)

"))

(th~defproblem _3140_B_THM (in landau3)
 (conclusion conc _3140_B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T54 _T57 _TH1)

"))

(th~defproblem _T58_THM (in landau3)
 (conclusion conc _T58)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_3140_B _3140_A _ISI1)

"))

(th~defproblem _SATZ140A_THM (in landau3)
 (conclusion conc _SATZ140A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T58 _SOMEI)

"))

(th~defproblem _T59_THM (in landau3)
 (conclusion conc _T59)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140B)

"))

(th~defproblem _SATZ140_THM (in landau3)
 (conclusion conc _SATZ140)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140A _T59 _ONEI)

"))

(th~defproblem _SATZ140C_THM (in landau3)
 (conclusion conc _SATZ140C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140)

"))

(th~defproblem _SATZ140D_THM (in landau3)
 (conclusion conc _SATZ140D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140C _SYMIS)

"))

(th~defproblem _SATZ140E_THM (in landau3)
 (conclusion conc _SATZ140E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140C _RP_COMPL _TRIS)

"))

(th~defproblem _SATZ140F_THM (in landau3)
 (conclusion conc _SATZ140F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140E _SYMIS)

"))

(th~defproblem _SATZ140G_THM (in landau3)
 (conclusion conc _SATZ140G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140C _SATZ140B)

"))

(th~defproblem _T60_THM (in landau3)
 (conclusion conc _T60)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140C _SYMIS _RP_ISPL1 _TR3IS)

"))

(th~defproblem _RP_ISMN12_THM (in landau3)
 (conclusion conc _RP_ISMN12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T60 _SATZ140G)

"))

(th~defproblem _ISMN1_THM (in landau3)
 (conclusion conc _ISMN1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RP_ISMN12)

"))

(th~defproblem _ISMN2_THM (in landau3)
 (conclusion conc _ISMN2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RP_ISMN12)

"))

(th~defproblem _III4_T1_THM (in landau3)
 (conclusion conc _III4_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3I)

"))

(th~defproblem _III4_T2_THM (in landau3)
 (conclusion conc _III4_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III4_T1 _SOMEI)

"))

(th~defproblem _III4_T3_THM (in landau3)
 (conclusion conc _III4_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III4_T2 _SOMEI)

"))

(th~defproblem _PROD1_THM (in landau3)
 (conclusion conc _PROD1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III4_T3 _ESTII)

"))

(th~defproblem _III4_T4_THM (in landau3)
 (conclusion conc _III4_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _III4_T5_THM (in landau3)
 (conclusion conc _III4_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E1)

"))

(th~defproblem _III4_T6_THM (in landau3)
 (conclusion conc _III4_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E2)

"))

(th~defproblem _III4_T7_THM (in landau3)
 (conclusion conc _III4_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E3)

"))

(th~defproblem _III4_T8_THM (in landau3)
 (conclusion conc _III4_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III4_T7 _III4_T6 _III4_T5)

"))

(th~defproblem _III4_T9_THM (in landau3)
 (conclusion conc _III4_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III4_T8 _SOMEAPP)

"))

(th~defproblem _PRODAPP_THM (in landau3)
 (conclusion conc _PRODAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III4_T9 _III4_T4 _SOMEAPP)

"))

(th~defproblem _4141_T1_THM (in landau3)
 (conclusion conc _4141_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A)

"))

(th~defproblem _4141_T2_THM (in landau3)
 (conclusion conc _4141_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A)

"))

(th~defproblem _4141_T3_THM (in landau3)
 (conclusion conc _4141_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T2 _4141_T1 _SATZ107A _SYMIS _RT_ISLESS1)

"))

(th~defproblem _4141_T4_THM (in landau3)
 (conclusion conc _4141_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T3 _SATZ81B _EC3E31)

"))

(th~defproblem _4141_T5_THM (in landau3)
 (conclusion conc _4141_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T4 _PRODAPP)

"))

(th~defproblem _SATZ141A_THM (in landau3)
 (conclusion conc _SATZ141A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T5 _REFIS _WELI _TH3)

"))

(th~defproblem _4141_T6_THM (in landau3)
 (conclusion conc _4141_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1C _SATZ110C _RT_ISTS1 _RT_ASSTS2 _TR3IS)

"))

(th~defproblem _SATZ141B_THM (in landau3)
 (conclusion conc _SATZ141B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T6 _SATZ110G)

"))

(th~defproblem _SATZ141C_THM (in landau3)
 (conclusion conc _SATZ141C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ141B _SYMIS)

"))

(th~defproblem _4141_T7_THM (in landau3)
 (conclusion conc _4141_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISLESS2)

"))

(th~defproblem _4141_T8_THM (in landau3)
 (conclusion conc _4141_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1C _SATZ110E _RT_ISTS1 _RT_ASSTS2 _TR3IS)

"))

(th~defproblem _4141_T9_THM (in landau3)
 (conclusion conc _4141_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T7 _SATZ105F _4141_T8 _SATZ141B _RT_ISLESS12)

"))

(th~defproblem _4141_T10_THM (in landau3)
 (conclusion conc _4141_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T9 _SATZ120)

"))

(th~defproblem _4141_T11_THM (in landau3)
 (conclusion conc _4141_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110D _4141_T10 _PROD1)

"))

(th~defproblem _4141_T12_THM (in landau3)
 (conclusion conc _4141_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T11 _PRODAPP)

"))

(th~defproblem _4141_T13_THM (in landau3)
 (conclusion conc _4141_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _PROD1)

"))

(th~defproblem _4141_T14_THM (in landau3)
 (conclusion conc _4141_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ105A)

"))

(th~defproblem _4141_T15_THM (in landau3)
 (conclusion conc _4141_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T14 _SYMIS _RT_ISMORE2)

"))

(th~defproblem _4141_T16_THM (in landau3)
 (conclusion conc _4141_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T15 _4141_T13 _ANDI)

"))

(th~defproblem _4141_T17_THM (in landau3)
 (conclusion conc _4141_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T16 _SOMEI)

"))

(th~defproblem _4141_T18_THM (in landau3)
 (conclusion conc _4141_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T17 _CUTAPP3)

"))

(th~defproblem _4141_T19_THM (in landau3)
 (conclusion conc _4141_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T18 _PRODAPP)

"))

(th~defproblem _4141_T20_THM (in landau3)
 (conclusion conc _4141_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T19 _4141_T12 _SATZ141A _REFIS _PROD1 _CUT2)

"))

(th~defproblem _4141_T21_THM (in landau3)
 (conclusion conc _4141_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T20 _CUTAPP1B)

"))

(th~defproblem _4141_T22_THM (in landau3)
 (conclusion conc _4141_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T21 _CUTAPP1B)

"))

(th~defproblem _4141_T23_THM (in landau3)
 (conclusion conc _4141_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T22 _CUTAPP1A)

"))

(th~defproblem _SATZ141_THM (in landau3)
 (conclusion conc _SATZ141)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4141_T23 _CUTAPP1A)

"))

(th~defproblem _LRTTS_THM (in landau3)
 (conclusion conc _LRTTS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PROD1 _SATZ141 _INE)

"))

(th~defproblem _III4_T10_THM (in landau3)
 (conclusion conc _III4_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ141A _ISP1)

"))

(th~defproblem _URTTS_THM (in landau3)
 (conclusion conc _URTTS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ141 _INI _III4_T10 _TH3)

"))

(th~defproblem _III4_T11_THM (in landau3)
 (conclusion conc _III4_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ141 _INI)

"))

(th~defproblem _TSAPP_THM (in landau3)
 (conclusion conc _TSAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III4_T11 _PRODAPP)

"))

(th~defproblem _RP_ISTS1_THM (in landau3)
 (conclusion conc _RP_ISTS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RP_ISTS2_THM (in landau3)
 (conclusion conc _RP_ISTS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RP_ISTS12_THM (in landau3)
 (conclusion conc _RP_ISTS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISTS2 _RP_ISTS1 _TRIS)

"))

(th~defproblem _4142_T1_THM (in landau3)
 (conclusion conc _4142_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_COMTS _TRIS)

"))

(th~defproblem _4142_T2_THM (in landau3)
 (conclusion conc _4142_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4142_T1 _LRTTS)

"))

(th~defproblem _4142_T3_THM (in landau3)
 (conclusion conc _4142_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4142_T2 _TSAPP)

"))

(th~defproblem _SATZ142_THM (in landau3)
 (conclusion conc _SATZ142)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4142_T3 _ISI1)

"))

(th~defproblem _RP_COMTS_THM (in landau3)
 (conclusion conc _RP_COMTS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ142)

"))

(th~defproblem _4143_T1_THM (in landau3)
 (conclusion conc _4143_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ASSTS1 _RT_ISTS1 _TR3IS)

"))

(th~defproblem _4143_T2_THM (in landau3)
 (conclusion conc _4143_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _LRTTS)

"))

(th~defproblem _4143_T3_THM (in landau3)
 (conclusion conc _4143_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4143_T1 _4143_T2 _LRTTS)

"))

(th~defproblem _4143_T4_THM (in landau3)
 (conclusion conc _4143_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4143_T3 _TSAPP)

"))

(th~defproblem _4143_T5_THM (in landau3)
 (conclusion conc _4143_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4143_T4 _TSAPP)

"))

(th~defproblem _4143_T6_THM (in landau3)
 (conclusion conc _4143_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ASSTS2 _RT_ISTS2 _TR3IS)

"))

(th~defproblem _4143_T7_THM (in landau3)
 (conclusion conc _4143_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _LRTTS)

"))

(th~defproblem _4143_T8_THM (in landau3)
 (conclusion conc _4143_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4143_T6 _4143_T7 _LRTTS)

"))

(th~defproblem _4143_T9_THM (in landau3)
 (conclusion conc _4143_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4143_T8 _TSAPP)

"))

(th~defproblem _4143_T10_THM (in landau3)
 (conclusion conc _4143_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4143_T9 _TSAPP)

"))

(th~defproblem _SATZ143_THM (in landau3)
 (conclusion conc _SATZ143)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4143_T10 _4143_T5 _ISI1)

"))

(th~defproblem _RP_ASSTS1_THM (in landau3)
 (conclusion conc _RP_ASSTS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ143)

"))

(th~defproblem _RP_ASSTS2_THM (in landau3)
 (conclusion conc _RP_ASSTS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ143 _SYMIS)

"))

(th~defproblem _4144_T1_THM (in landau3)
 (conclusion conc _4144_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_DISTTP2 _RT_ISTS2 _TR3IS)

"))

(th~defproblem _4144_T2_THM (in landau3)
 (conclusion conc _4144_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _LRTTS)

"))

(th~defproblem _4144_T3_THM (in landau3)
 (conclusion conc _4144_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _LRTTS)

"))

(th~defproblem _4144_T4_THM (in landau3)
 (conclusion conc _4144_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T1 _4144_T3 _4144_T2 _LRTPL)

"))

(th~defproblem _4144_T5_THM (in landau3)
 (conclusion conc _4144_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T4 _PLAPP)

"))

(th~defproblem _4144_T6_THM (in landau3)
 (conclusion conc _4144_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T5 _TSAPP)

"))

(th~defproblem _4144_T7_THM (in landau3)
 (conclusion conc _4144_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISPL12 _TRIS)

"))

(th~defproblem _4144_T8_THM (in landau3)
 (conclusion conc _4144_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITET)

"))

(th~defproblem _4144_T9_THM (in landau3)
 (conclusion conc _4144_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T8 _ISP1)

"))

(th~defproblem _4144_T10_THM (in landau3)
 (conclusion conc _4144_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T8 _SYMIS _RT_LESSISI2)

"))

(th~defproblem _4144_T11_THM (in landau3)
 (conclusion conc _4144_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T10 _SATZ84 _SATZ88)

"))

(th~defproblem _4144_T12_THM (in landau3)
 (conclusion conc _4144_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITEF)

"))

(th~defproblem _4144_T13_THM (in landau3)
 (conclusion conc _4144_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T12 _ISP1)

"))

(th~defproblem _4144_T14_THM (in landau3)
 (conclusion conc _4144_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T12 _SYMIS _RT_LESSISI2)

"))

(th~defproblem _4144_T15_THM (in landau3)
 (conclusion conc _4144_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T14 _SATZ81J _SATZ87B _RT_LESSISI1)

"))

(th~defproblem _4144_T16_THM (in landau3)
 (conclusion conc _4144_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T13 _4144_T9 _TH1)

"))

(th~defproblem _4144_T17_THM (in landau3)
 (conclusion conc _4144_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T15 _4144_T10 _TH1)

"))

(th~defproblem _4144_T18_THM (in landau3)
 (conclusion conc _4144_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T14 _4144_T11 _TH1)

"))

(th~defproblem _4144_T19_THM (in landau3)
 (conclusion conc _4144_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _LRTPL)

"))

(th~defproblem _4144_T20_THM (in landau3)
 (conclusion conc _4144_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _4144_T19 _4144_T16 _LRTTS)

"))

(th~defproblem _4144_T21_THM (in landau3)
 (conclusion conc _4144_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RT_LESSISI2 _4144_T17 _SATZ109A)

"))

(th~defproblem _4144_T22_THM (in landau3)
 (conclusion conc _4144_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RT_LESSISI2 _4144_T18 _SATZ109A)

"))

(th~defproblem _4144_T23_THM (in landau3)
 (conclusion conc _4144_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T22 _4144_T21 _SATZ100A _RT_DISTPT2 _4144_T7 _SYMIS _RT_ISLESSIS12)

"))

(th~defproblem _4144_T24_THM (in landau3)
 (conclusion conc _4144_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T20 _ISP1 _SATZ120 _4144_T23 _ORAPP)

"))

(th~defproblem _4144_T25_THM (in landau3)
 (conclusion conc _4144_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T24 _TSAPP)

"))

(th~defproblem _4144_T26_THM (in landau3)
 (conclusion conc _4144_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T25 _TSAPP)

"))

(th~defproblem _4144_T27_THM (in landau3)
 (conclusion conc _4144_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T26 _PLAPP)

"))

(th~defproblem _SATZ144_THM (in landau3)
 (conclusion conc _SATZ144)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4144_T27 _4144_T6 _ISI1)

"))

(th~defproblem _RP_DISTTP1_THM (in landau3)
 (conclusion conc _RP_DISTTP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMTS _RP_ISPL12 _SATZ144 _TR3IS)

"))

(th~defproblem _RP_DISTTP2_THM (in landau3)
 (conclusion conc _RP_DISTTP2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ144)

"))

(th~defproblem _RP_DISTPT1_THM (in landau3)
 (conclusion conc _RP_DISTPT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_DISTTP1 _SYMIS)

"))

(th~defproblem _RP_DISTPT2_THM (in landau3)
 (conclusion conc _RP_DISTPT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_DISTTP2 _SYMIS)

"))

(th~defproblem _4145_T1_THM (in landau3)
 (conclusion conc _4145_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ140D)

"))

(th~defproblem _4145_T2_THM (in landau3)
 (conclusion conc _4145_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_DISTTP1 _4145_T1 _RP_ISTS1 _TRIS)

"))

(th~defproblem _SATZ145A_THM (in landau3)
 (conclusion conc _SATZ145A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ133 _4145_T2 _SYMIS _RP_ISMORE1)

"))

(th~defproblem _SATZ145B_THM (in landau3)
 (conclusion conc _SATZ145B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISTS1)

"))

(th~defproblem _SATZ145C_THM (in landau3)
 (conclusion conc _SATZ145C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122 _SATZ145A _SATZ121)

"))

(th~defproblem _SATZ145D_THM (in landau3)
 (conclusion conc _SATZ145D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145A _RP_COMTS _RP_ISMORE12)

"))

(th~defproblem _SATZ145E_THM (in landau3)
 (conclusion conc _SATZ145E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISTS2)

"))

(th~defproblem _SATZ145F_THM (in landau3)
 (conclusion conc _SATZ145F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145C _RP_COMTS _RP_ISLESS12)

"))

(th~defproblem _SATZ145G_THM (in landau3)
 (conclusion conc _SATZ145G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145D _RP_ISTS1 _RP_ISMORE2)

"))

(th~defproblem _SATZ145H_THM (in landau3)
 (conclusion conc _SATZ145H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145G _RP_COMTS _RP_ISMORE12)

"))

(th~defproblem _SATZ145J_THM (in landau3)
 (conclusion conc _SATZ145J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145F _RP_ISTS1 _RP_ISLESS2)

"))

(th~defproblem _SATZ145K_THM (in landau3)
 (conclusion conc _SATZ145K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145J _RP_COMTS _RP_ISLESS12)

"))

(th~defproblem _4146_T1_THM (in landau3)
 (conclusion conc _4146_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123A)

"))

(th~defproblem _4146_T2_THM (in landau3)
 (conclusion conc _4146_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123B)

"))

(th~defproblem _SATZ146A_THM (in landau3)
 (conclusion conc _SATZ146A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145C _SATZ145A _SATZ145B _4146_T2 _4146_T1 _EC3_TH11)

"))

(th~defproblem _SATZ146B_THM (in landau3)
 (conclusion conc _SATZ146B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145C _SATZ145A _SATZ145B _4146_T2 _4146_T1 _EC3_TH10)

"))

(th~defproblem _SATZ146C_THM (in landau3)
 (conclusion conc _SATZ146C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145C _SATZ145A _SATZ145B _4146_T2 _4146_T1 _EC3_TH12)

"))

(th~defproblem _SATZ146D_THM (in landau3)
 (conclusion conc _SATZ146D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMTS _RP_ISMORE12 _SATZ146A)

"))

(th~defproblem _SATZ146E_THM (in landau3)
 (conclusion conc _SATZ146E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMTS _TR3IS _SATZ146B)

"))

(th~defproblem _SATZ146F_THM (in landau3)
 (conclusion conc _SATZ146F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMTS _RP_ISLESS12 _SATZ146C)

"))

(th~defproblem _4147_T1_THM (in landau3)
 (conclusion conc _4147_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145A)

"))

(th~defproblem _4147_T2_THM (in landau3)
 (conclusion conc _4147_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145A _RP_COMTS _RP_ISMORE12)

"))

(th~defproblem _SATZ147_THM (in landau3)
 (conclusion conc _SATZ147)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4147_T2 _4147_T1 _RP_TRMORE)

"))

(th~defproblem _SATZ147A_THM (in landau3)
 (conclusion conc _SATZ147A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122 _SATZ147 _SATZ121)

"))

(th~defproblem _SATZ148A_THM (in landau3)
 (conclusion conc _SATZ148A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145G _SATZ147 _ORAPP)

"))

(th~defproblem _SATZ148B_THM (in landau3)
 (conclusion conc _SATZ148B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145H _SATZ147 _ORAPP)

"))

(th~defproblem _SATZ148C_THM (in landau3)
 (conclusion conc _SATZ148C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122 _SATZ125 _SATZ148A _SATZ121)

"))

(th~defproblem _SATZ148D_THM (in landau3)
 (conclusion conc _SATZ148D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ125 _SATZ122 _SATZ148B _SATZ121)

"))

(th~defproblem _4149_T1_THM (in landau3)
 (conclusion conc _4149_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_ISTS12 _RP_MOREISI2)

"))

(th~defproblem _4149_T2_THM (in landau3)
 (conclusion conc _4149_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ148A _RP_MOREISI1)

"))

(th~defproblem _4149_T3_THM (in landau3)
 (conclusion conc _4149_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4149_T1 _4149_T2 _ORAPP)

"))

(th~defproblem _4149_T4_THM (in landau3)
 (conclusion conc _4149_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ148B _RP_MOREISI1)

"))

(th~defproblem _SATZ149_THM (in landau3)
 (conclusion conc _SATZ149)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4149_T3 _4149_T4 _ORAPP)

"))

(th~defproblem _SATZ149A_THM (in landau3)
 (conclusion conc _SATZ149A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ125 _SATZ149 _SATZ124)

"))

(th~defproblem _4150_T1_THM (in landau3)
 (conclusion conc _4150_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ90)

"))

(th~defproblem _4150_T2_THM (in landau3)
 (conclusion conc _4150_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTII)

"))

(th~defproblem _4150_T3_THM (in landau3)
 (conclusion conc _4150_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _SATZ81B _EC3E13)

"))

(th~defproblem _4150_T4_THM (in landau3)
 (conclusion conc _4150_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE _4150_T3 _TH3)

"))

(th~defproblem _4150_T5_THM (in landau3)
 (conclusion conc _4150_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _4150_T6_THM (in landau3)
 (conclusion conc _4150_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4150_T5 _RT_TRLESS _4150_T2)

"))

(th~defproblem _4150_T7_THM (in landau3)
 (conclusion conc _4150_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4150_T5 _SATZ91)

"))

(th~defproblem _4150_T8_THM (in landau3)
 (conclusion conc _4150_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _4150_T9_THM (in landau3)
 (conclusion conc _4150_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _4150_T10_THM (in landau3)
 (conclusion conc _4150_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4150_T8 _SATZ83 _4150_T9 _4150_T2 _ANDI)

"))

(th~defproblem _4150_T11_THM (in landau3)
 (conclusion conc _4150_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4150_T10 _4150_T7 _SOME_TH6)

"))

(th~defproblem _4150_T12_THM (in landau3)
 (conclusion conc _4150_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4150_T11 _4150_T6 _4150_T4 _4150_T2 _CUT2)

"))

(th~defproblem _SATZ150_THM (in landau3)
 (conclusion conc _SATZ150)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4150_T12 _4150_T1 _SOMEAPP)

"))

(th~defproblem _LRTRPOFRT_THM (in landau3)
 (conclusion conc _LRTRPOFRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4150_T2 _SATZ150 _INE)

"))

(th~defproblem _LRTRPOFRTE_THM (in landau3)
 (conclusion conc _LRTRPOFRTE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ150 _INI _4150_T5)

"))

(th~defproblem _III4_T12_THM (in landau3)
 (conclusion conc _III4_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81C)

"))

(th~defproblem _URTRPOFRT_THM (in landau3)
 (conclusion conc _URTRPOFRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE _III4_T12 _TH3)

"))

(th~defproblem _4151_T1_THM (in landau3)
 (conclusion conc _4151_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _4151_T2_THM (in landau3)
 (conclusion conc _4151_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4151_T1 _SATZ105F _EXAMPLE1A _SYMIS _RT_ISLESS12)

"))

(th~defproblem _4151_T3_THM (in landau3)
 (conclusion conc _4151_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4151_T2 _SATZ120)

"))

(th~defproblem _4151_T4_THM (in landau3)
 (conclusion conc _4151_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4151_T3 _TSAPP)

"))

(th~defproblem _4151_T5_THM (in landau3)
 (conclusion conc _4151_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ105F _SATZ110E _RT_ISLESS2)

"))

(th~defproblem _4151_T6_THM (in landau3)
 (conclusion conc _4151_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4151_T5 _LRTRPOFRT)

"))

(th~defproblem _4151_T7_THM (in landau3)
 (conclusion conc _4151_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1C _SATZ110C _RT_ISTS1 _RT_ASSTS2 _TR3IS)

"))

(th~defproblem _4151_T8_THM (in landau3)
 (conclusion conc _4151_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4151_T7 _SYMIS _4151_T6 _LRTTS)

"))

(th~defproblem _4151_T9_THM (in landau3)
 (conclusion conc _4151_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4151_T8 _CUTAPP3)

"))

(th~defproblem _SATZ151_THM (in landau3)
 (conclusion conc _SATZ151)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4151_T9 _4151_T4 _ISI1)

"))

(th~defproblem _SATZ151A_THM (in landau3)
 (conclusion conc _SATZ151A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ151 _SYMIS)

"))

(th~defproblem _SATZ151B_THM (in landau3)
 (conclusion conc _SATZ151B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ151 _RP_COMTS _TRIS)

"))

(th~defproblem _SATZ151C_THM (in landau3)
 (conclusion conc _SATZ151C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ151B _SYMIS)

"))

(th~defproblem _4152_T1_THM (in landau3)
 (conclusion conc _4152_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDI)

"))

(th~defproblem _4152_T2_THM (in landau3)
 (conclusion conc _4152_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T1 _SOMEI)

"))

(th~defproblem _4152_T3_THM (in landau3)
 (conclusion conc _4152_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T2 _AND3I)

"))

(th~defproblem _4152_T4_THM (in landau3)
 (conclusion conc _4152_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T3 _SOMEI)

"))

(th~defproblem _INV1_THM (in landau3)
 (conclusion conc _INV1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T4 _ESTII)

"))

(th~defproblem _4152_T5_THM (in landau3)
 (conclusion conc _4152_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _4152_T6_THM (in landau3)
 (conclusion conc _4152_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E1)

"))

(th~defproblem _4152_T7_THM (in landau3)
 (conclusion conc _4152_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E2)

"))

(th~defproblem _4152_T8_THM (in landau3)
 (conclusion conc _4152_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E3)

"))

(th~defproblem _4152_T9_THM (in landau3)
 (conclusion conc _4152_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _4152_T10_THM (in landau3)
 (conclusion conc _4152_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _4152_T11_THM (in landau3)
 (conclusion conc _4152_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T8 _4152_T10 _4152_T9 _4152_T6)

"))

(th~defproblem _4152_T12_THM (in landau3)
 (conclusion conc _4152_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T11 _4152_T7 _SOMEAPP)

"))

(th~defproblem _INVAPP_THM (in landau3)
 (conclusion conc _INVAPP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T12 _4152_T5 _SOMEAPP)

"))

(th~defproblem _4152_T13_THM (in landau3)
 (conclusion conc _4152_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ94A)

"))

(th~defproblem _4152_T14_THM (in landau3)
 (conclusion conc _4152_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T13 _SATZ119A)

"))

(th~defproblem _4152_T15_THM (in landau3)
 (conclusion conc _4152_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _4152_T13 _4152_T14 _INV1)

"))

(th~defproblem _4152_T16_THM (in landau3)
 (conclusion conc _4152_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP1 _TH3)

"))

(th~defproblem _4152_T17_THM (in landau3)
 (conclusion conc _4152_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110E)

"))

(th~defproblem _4152_T18_THM (in landau3)
 (conclusion conc _4152_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110E)

"))

(th~defproblem _4152_T19_THM (in landau3)
 (conclusion conc _4152_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T18 _RT_ISTS1 _TRIS)

"))

(th~defproblem _4152_T20_THM (in landau3)
 (conclusion conc _4152_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T19 _4152_T17 _SATZ110B)

"))

(th~defproblem _4152_T21_THM (in landau3)
 (conclusion conc _4152_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T20 _4152_T16 _TH3)

"))

(th~defproblem _4152_T22_THM (in landau3)
 (conclusion conc _4152_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS _4152_T21 _INVAPP)

"))

(th~defproblem _4152_T23_THM (in landau3)
 (conclusion conc _4152_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T22)

"))

(th~defproblem _4152_T24_THM (in landau3)
 (conclusion conc _4152_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISLESS2)

"))

(th~defproblem _4152_T25_THM (in landau3)
 (conclusion conc _4152_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110D _SATZ110E _TRIS)

"))

(th~defproblem _4152_T26_THM (in landau3)
 (conclusion conc _4152_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T24 _SATZ105C _4152_T25 _RT_ISLESS2)

"))

(th~defproblem _4152_T27_THM (in landau3)
 (conclusion conc _4152_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T26 _RT_COMTS _RT_ISLESS12)

"))

(th~defproblem _4152_T28_THM (in landau3)
 (conclusion conc _4152_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T27 _SATZ106C)

"))

(th~defproblem _4152_T29_THM (in landau3)
 (conclusion conc _4152_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T28 _SATZ119A)

"))

(th~defproblem _4152_T30_THM (in landau3)
 (conclusion conc _4152_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110E)

"))

(th~defproblem _4152_T31_THM (in landau3)
 (conclusion conc _4152_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T30 _SATZ110G)

"))

(th~defproblem _4152_T32_THM (in landau3)
 (conclusion conc _4152_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T31 _4152_T28 _4152_T29 _INV1)

"))

(th~defproblem _4152_T33_THM (in landau3)
 (conclusion conc _4152_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T32 _INVAPP)

"))

(th~defproblem _4152_T34_THM (in landau3)
 (conclusion conc _4152_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ91)

"))

(th~defproblem _4152_T35_THM (in landau3)
 (conclusion conc _4152_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _4152_T36_THM (in landau3)
 (conclusion conc _4152_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T35 _SATZ119A)

"))

(th~defproblem _4152_T37_THM (in landau3)
 (conclusion conc _4152_T37)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _4152_T35 _4152_T36 _INV1)

"))

(th~defproblem _4152_T38_THM (in landau3)
 (conclusion conc _4152_T38)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _4152_T39_THM (in landau3)
 (conclusion conc _4152_T39)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110D _SATZ110C _TRIS)

"))

(th~defproblem _4152_T40_THM (in landau3)
 (conclusion conc _4152_T40)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T38 _SATZ105C _4152_T39 _RT_ISLESS2)

"))

(th~defproblem _4152_T41_THM (in landau3)
 (conclusion conc _4152_T41)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T40 _RT_COMTS _RT_ISLESS12)

"))

(th~defproblem _4152_T42_THM (in landau3)
 (conclusion conc _4152_T42)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T41 _SATZ106C)

"))

(th~defproblem _4152_T43_THM (in landau3)
 (conclusion conc _4152_T43)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T42 _SATZ83 _SYMIS _RT_ISMORE2)

"))

(th~defproblem _4152_T44_THM (in landau3)
 (conclusion conc _4152_T44)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T43 _4152_T37 _ANDI)

"))

(th~defproblem _4152_T45_THM (in landau3)
 (conclusion conc _4152_T45)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T44 _SOMEI)

"))

(th~defproblem _4152_T46_THM (in landau3)
 (conclusion conc _4152_T46)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T45 _4152_T34 _SOMEAPP)

"))

(th~defproblem _4152_T47_THM (in landau3)
 (conclusion conc _4152_T47)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T46 _INVAPP)

"))

(th~defproblem _4152_T48_THM (in landau3)
 (conclusion conc _4152_T48)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T47 _4152_T33 _4152_T23 _4152_T15 _CUT2)

"))

(th~defproblem _4152_T49_THM (in landau3)
 (conclusion conc _4152_T49)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T48 _CUTAPP1B)

"))

(th~defproblem _4152_T50_THM (in landau3)
 (conclusion conc _4152_T50)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T49 _CUTAPP1A)

"))

(th~defproblem _4152_T51_THM (in landau3)
 (conclusion conc _4152_T51)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISTS2 _TRIS)

"))

(th~defproblem _4152_T52_THM (in landau3)
 (conclusion conc _4152_T52)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A)

"))

(th~defproblem _4152_T53_THM (in landau3)
 (conclusion conc _4152_T53)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T52 _SATZ105C _SATZ110C _4152_T51 _SYMIS _RT_ISLESS12)

"))

(th~defproblem _4152_T54_THM (in landau3)
 (conclusion conc _4152_T54)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T53 _LRTRPOFRT)

"))

(th~defproblem _R1_THM (in landau3)
 (conclusion conc _R1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T50 _INI)

"))

(th~defproblem _R2_THM (in landau3)
 (conclusion conc _R2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T54 _R1 _INVAPP)

"))

(th~defproblem _R3_THM (in landau3)
 (conclusion conc _R3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_R2 _TSAPP)

"))

(th~defproblem _4152_T55_THM (in landau3)
 (conclusion conc _4152_T55)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _4152_T56_THM (in landau3)
 (conclusion conc _4152_T56)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T55 _SATZ83)

"))

(th~defproblem _4152_T57_THM (in landau3)
 (conclusion conc _4152_T57)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2B)

"))

(th~defproblem _4152_T58_THM (in landau3)
 (conclusion conc _4152_T58)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_CUTAPP2A _4152_T56 _4152_T57)

"))

(th~defproblem _4152_T59_THM (in landau3)
 (conclusion conc _4152_T59)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T58 _4152_T56 _SATZ105F _4152_T57)

"))

(th~defproblem _4152_T60_THM (in landau3)
 (conclusion conc _4152_T60)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T59 _4152_T56 _4152_T57 _SYMIS _RT_ISLESS1)

"))

(th~defproblem _T61_THM (in landau3)
 (conclusion conc _T61)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T57 _SATZ101F _EXAMPLE1C _4152_T56 _SATZ101E _RT_ISTS1 _RT_DISTPT1 _TR4IS)

"))

(th~defproblem _T62_THM (in landau3)
 (conclusion conc _T62)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4152_T60 _4152_T56 _4152_T57 _SATZ96C)

"))

(th~defproblem _T63_THM (in landau3)
 (conclusion conc _T63)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T62 _T61 _4152_T57 _4152_T56 _RT_ISLESS2)

"))

(th~defproblem _T64_THM (in landau3)
 (conclusion conc _T64)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T63 _4152_T57 _RT_COMPL _RT_ISLESS12 _4152_T56)

"))

(th~defproblem _T65_THM (in landau3)
 (conclusion conc _T65)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T64 _4152_T57 _SATZ97C _4152_T56)

"))

(th~defproblem _T66_THM (in landau3)
 (conclusion conc _T66)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1C _SATZ110E _RT_ISTS1 _RT_ASSTS2 _TR3IS _4152_T56 _4152_T57)

"))

(th~defproblem _T67_THM (in landau3)
 (conclusion conc _T67)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T65 _SATZ105F _SATZ141B _T66 _RT_ISLESS12 _4152_T56 _4152_T57)

"))

(th~defproblem _T68_THM (in landau3)
 (conclusion conc _T68)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T67 _SATZ119A _4152_T56 _4152_T57)

"))

(th~defproblem _T69_THM (in landau3)
 (conclusion conc _T69)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110E _4152_T56 _4152_T57)

"))

(th~defproblem _T70_THM (in landau3)
 (conclusion conc _T70)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_COMTS _SATZ141C _T69 _SATZ110G _TR3IS _4152_T56 _4152_T57)

"))

(th~defproblem _T71_THM (in landau3)
 (conclusion conc _T71)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _T67 _T68 _INV1 _4152_T56 _4152_T57)

"))

(th~defproblem _T72_THM (in landau3)
 (conclusion conc _T72)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T71 _4152_T50 _INE _4152_T56 _4152_T57)

"))

(th~defproblem _T73_THM (in landau3)
 (conclusion conc _T73)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T70 _T72 _LRTTS _4152_T56 _4152_T57)

"))

(th~defproblem _T74_THM (in landau3)
 (conclusion conc _T74)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T73 _4152_T56 _CUTAPP2B _SATZ132APP)

"))

(th~defproblem _T75_THM (in landau3)
 (conclusion conc _T75)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T74 _CUTAPP1A)

"))

(th~defproblem _T76_THM (in landau3)
 (conclusion conc _T76)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T75 _R3 _ISI1)

"))

(th~defproblem _SATZ152_THM (in landau3)
 (conclusion conc _SATZ152)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T76 _SOMEI)

"))

(th~defproblem _4153_T1_THM (in landau3)
 (conclusion conc _4153_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145D)

"))

(th~defproblem _4153_T2_THM (in landau3)
 (conclusion conc _4153_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4153_T1 _SATZ123B _EC3E21)

"))

(th~defproblem _4153_T3_THM (in landau3)
 (conclusion conc _4153_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ145F)

"))

(th~defproblem _4153_T4_THM (in landau3)
 (conclusion conc _4153_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4153_T3 _SATZ123B _EC3E31)

"))

(th~defproblem _4153_T5_THM (in landau3)
 (conclusion conc _4153_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123A _OR3_TH1)

"))

(th~defproblem _4153_T6_THM (in landau3)
 (conclusion conc _4153_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4153_T4 _4153_T2 _4153_T5 _ORAPP)

"))

(th~defproblem _SATZ153B_THM (in landau3)
 (conclusion conc _SATZ153B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4153_T6 _TRIS2 _WELI _TH7)

"))

(th~defproblem _4153_T7_THM (in landau3)
 (conclusion conc _4153_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ151B _RP_ISTS1 _RP_ASSTS2 _TR3IS)

"))

(th~defproblem _4153_T8_THM (in landau3)
 (conclusion conc _4153_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4153_T7 _SOMEI)

"))

(th~defproblem _SATZ153A_THM (in landau3)
 (conclusion conc _SATZ153A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_4153_T8 _SATZ152 _SOMEAPP)

"))

(th~defproblem _4153_T9_THM (in landau3)
 (conclusion conc _4153_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153B)

"))

(th~defproblem _SATZ153_THM (in landau3)
 (conclusion conc _SATZ153)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153A _4153_T9 _ONEI)

"))

(th~defproblem _SATZ153C_THM (in landau3)
 (conclusion conc _SATZ153C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153)

"))

(th~defproblem _SATZ153D_THM (in landau3)
 (conclusion conc _SATZ153D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153C _SYMIS)

"))

(th~defproblem _SATZ153E_THM (in landau3)
 (conclusion conc _SATZ153E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153C _RP_COMTS _TRIS)

"))

(th~defproblem _SATZ153F_THM (in landau3)
 (conclusion conc _SATZ153F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153E _SYMIS)

"))

(th~defproblem _SATZ153G_THM (in landau3)
 (conclusion conc _SATZ153G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153C _SATZ153B)

"))

(th~defproblem _RATRPI_THM (in landau3)
 (conclusion conc _RATRPI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IMAGEI)

"))

(th~defproblem _NATRPI_THM (in landau3)
 (conclusion conc _NATRPI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IMAGEI)

"))

(th~defproblem _III5_T1_THM (in landau3)
 (conclusion conc _III5_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SOMEI)

"))

(th~defproblem _LEMMAIII5_THM (in landau3)
 (conclusion conc _LEMMAIII5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T1 _SOMEAPP)

"))

(th~defproblem _5154_T1_THM (in landau3)
 (conclusion conc _5154_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ82 _LRTRPOFRT)

"))

(th~defproblem _5154_T2_THM (in landau3)
 (conclusion conc _5154_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RT_MOREISI2 _URTRPOFRT)

"))

(th~defproblem _5154_T3_THM (in landau3)
 (conclusion conc _5154_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5154_T2 _5154_T1 _ANDI)

"))

(th~defproblem _SATZ154A_THM (in landau3)
 (conclusion conc _SATZ154A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5154_T3 _SOMEI)

"))

(th~defproblem _SATZ154B_THM (in landau3)
 (conclusion conc _SATZ154B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _SATZ154C_THM (in landau3)
 (conclusion conc _SATZ154C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ154A _SATZ121)

"))

(th~defproblem _5154_T4_THM (in landau3)
 (conclusion conc _5154_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81A)

"))

(th~defproblem _5154_T5_THM (in landau3)
 (conclusion conc _5154_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123B)

"))

(th~defproblem _SATZ154D_THM (in landau3)
 (conclusion conc _SATZ154D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154C _SATZ154A _SATZ154B _5154_T5 _5154_T4 _EC3_TH11)

"))

(th~defproblem _SATZ154E_THM (in landau3)
 (conclusion conc _SATZ154E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154C _SATZ154A _SATZ154B _5154_T5 _5154_T4 _EC3_TH10)

"))

(th~defproblem _SATZ154F_THM (in landau3)
 (conclusion conc _SATZ154F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154C _SATZ154A _SATZ154B _5154_T5 _5154_T4 _EC3_TH12)

"))

(th~defproblem _III5_T2_THM (in landau3)
 (conclusion conc _III5_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154E)

"))

(th~defproblem _ISRTERP_THM (in landau3)
 (conclusion conc _ISRTERP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154B)

"))

(th~defproblem _ISRTIRP_THM (in landau3)
 (conclusion conc _ISRTIRP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154E)

"))

(th~defproblem _ISRPERT_THM (in landau3)
 (conclusion conc _ISRPERT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T2 _ISINV)

"))

(th~defproblem _ISRPIRT_THM (in landau3)
 (conclusion conc _ISRPIRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T2 _ISINVE)

"))

(th~defproblem _ISRTRP1_THM (in landau3)
 (conclusion conc _ISRTRP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T2 _ISST1 _RATRPI)

"))

(th~defproblem _ISRTRP2_THM (in landau3)
 (conclusion conc _ISRTRP2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T2 _ISST2 _RATRPI)

"))

(th~defproblem _ISRPRT1_THM (in landau3)
 (conclusion conc _ISRPRT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T2 _ISTS1)

"))

(th~defproblem _ISRPRT2_THM (in landau3)
 (conclusion conc _ISRPRT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T2 _ISTS2)

"))

(th~defproblem _ISNTERP_THM (in landau3)
 (conclusion conc _ISNTERP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _ISNTIRP_THM (in landau3)
 (conclusion conc _ISNTIRP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISRTIRP _ISNIRT)

"))

(th~defproblem _III5_T3_THM (in landau3)
 (conclusion conc _III5_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTIRP)

"))

(th~defproblem _ISRPENT_THM (in landau3)
 (conclusion conc _ISRPENT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T3 _ISINV)

"))

(th~defproblem _ISRPINT_THM (in landau3)
 (conclusion conc _ISRPINT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T3 _ISINVE)

"))

(th~defproblem _ISNTRP1_THM (in landau3)
 (conclusion conc _ISNTRP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T3 _ISST1 _NATRPI)

"))

(th~defproblem _ISNTRP2_THM (in landau3)
 (conclusion conc _ISNTRP2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T3 _ISST2 _NATRPI)

"))

(th~defproblem _ISRPNT1_THM (in landau3)
 (conclusion conc _ISRPNT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T3 _ISTS1)

"))

(th~defproblem _ISRPNT2_THM (in landau3)
 (conclusion conc _ISRPNT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T3 _ISTS2)

"))

(th~defproblem _5155_T1_THM (in landau3)
 (conclusion conc _5155_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _5155_T2_THM (in landau3)
 (conclusion conc _5155_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _5155_T3_THM (in landau3)
 (conclusion conc _5155_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T2 _5155_T1 _SATZ98A)

"))

(th~defproblem _5155_T4_THM (in landau3)
 (conclusion conc _5155_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T3 _SYMIS _RT_ISLESS1)

"))

(th~defproblem _5155_T5_THM (in landau3)
 (conclusion conc _5155_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T4 _LRTRPOFRT)

"))

(th~defproblem _5155_T6_THM (in landau3)
 (conclusion conc _5155_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T5 _PLAPP)

"))

(th~defproblem _5155_T7_THM (in landau3)
 (conclusion conc _5155_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _5155_T8_THM (in landau3)
 (conclusion conc _5155_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T7 _EXAMPLE1D _SATZ110F _RT_ISLESS12)

"))

(th~defproblem _5155_T9_THM (in landau3)
 (conclusion conc _5155_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T8 _SATZ106C)

"))

(th~defproblem _5155_T10_THM (in landau3)
 (conclusion conc _5155_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_DISTTP1 _SATZ110D _TRIS)

"))

(th~defproblem _5155_T11_THM (in landau3)
 (conclusion conc _5155_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ105C _EXAMPLE1C _RT_COMTS _RT_ISLESS12)

"))

(th~defproblem _5155_T12_THM (in landau3)
 (conclusion conc _5155_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T11 _LRTRPOFRT)

"))

(th~defproblem _5155_T13_THM (in landau3)
 (conclusion conc _5155_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T10 _5155_T9 _5155_T12 _LRTPL)

"))

(th~defproblem _SATZ155A_THM (in landau3)
 (conclusion conc _SATZ155A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T6 _5155_T13 _ISI1)

"))

(th~defproblem _5155_T14_THM (in landau3)
 (conclusion conc _5155_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101F)

"))

(th~defproblem _5155_T15_THM (in landau3)
 (conclusion conc _5155_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ155A _5155_T14 _ISRTERP _TRIS)

"))

(th~defproblem _5155_T16_THM (in landau3)
 (conclusion conc _5155_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T15 _RP_COMPL _TRIS2)

"))

(th~defproblem _SATZ155B_THM (in landau3)
 (conclusion conc _SATZ155B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T16 _SATZ154A _SATZ140G)

"))

(th~defproblem _5155_T17_THM (in landau3)
 (conclusion conc _5155_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _5155_T18_THM (in landau3)
 (conclusion conc _5155_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _5155_T19_THM (in landau3)
 (conclusion conc _5155_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T18 _5155_T17 _SATZ107A)

"))

(th~defproblem _5155_T20_THM (in landau3)
 (conclusion conc _5155_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T19 _SYMIS _RT_ISLESS1)

"))

(th~defproblem _5155_T21_THM (in landau3)
 (conclusion conc _5155_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T20 _LRTRPOFRT)

"))

(th~defproblem _5155_T22_THM (in landau3)
 (conclusion conc _5155_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T21 _TSAPP)

"))

(th~defproblem _5155_T23_THM (in landau3)
 (conclusion conc _5155_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LRTRPOFRTE)

"))

(th~defproblem _5155_T24_THM (in landau3)
 (conclusion conc _5155_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _5155_T25_THM (in landau3)
 (conclusion conc _5155_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _5155_T26_THM (in landau3)
 (conclusion conc _5155_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T24 _EXAMPLE1D _SATZ110F _RT_ISLESS12)

"))

(th~defproblem _5155_T27_THM (in landau3)
 (conclusion conc _5155_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T26 _SATZ106C)

"))

(th~defproblem _5155_T28_THM (in landau3)
 (conclusion conc _5155_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T25 _SATZ110F _RT_ISLESS1)

"))

(th~defproblem _5155_T29_THM (in landau3)
 (conclusion conc _5155_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T28 _SATZ106C)

"))

(th~defproblem _5155_T30_THM (in landau3)
 (conclusion conc _5155_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ASSTS1 _SATZ110F _RT_ISTS1 _SATZ110D _TR3IS)

"))

(th~defproblem _5155_T31_THM (in landau3)
 (conclusion conc _5155_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T30 _5155_T27 _5155_T12 _5155_T29 _LRTRPOFRT _LRTTS)

"))

(th~defproblem _5155_T32_THM (in landau3)
 (conclusion conc _5155_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T31 _5155_T23 _SATZ91 _SOMEAPP)

"))

(th~defproblem _SATZ155C_THM (in landau3)
 (conclusion conc _SATZ155C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T22 _5155_T32 _ISI1)

"))

(th~defproblem _5155_T33_THM (in landau3)
 (conclusion conc _5155_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110F)

"))

(th~defproblem _5155_T34_THM (in landau3)
 (conclusion conc _5155_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ155C _5155_T33 _ISRTERP _TRIS)

"))

(th~defproblem _5155_T35_THM (in landau3)
 (conclusion conc _5155_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T34 _RP_COMTS _TRIS2)

"))

(th~defproblem _SATZ155D_THM (in landau3)
 (conclusion conc _SATZ155D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5155_T35 _SATZ153G)

"))

(th~defproblem _SATZ155E_THM (in landau3)
 (conclusion conc _SATZ155E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ155A _SATZ112H _SYMIS _ISRTERP _TRIS)

"))

(th~defproblem _SATZ155F_THM (in landau3)
 (conclusion conc _SATZ155F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ155C _SATZ112J _SYMIS _ISRTERP _TRIS)

"))

(th~defproblem _NT_NATRPI_THM (in landau3)
 (conclusion conc _NT_NATRPI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INP)

"))

(th~defproblem _ISRPENTT_THM (in landau3)
 (conclusion conc _ISRPENTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTI)

"))

(th~defproblem _ISRPINTT_THM (in landau3)
 (conclusion conc _ISRPINTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTE)

"))

(th~defproblem _ISNTTERP_THM (in landau3)
 (conclusion conc _ISNTTERP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINI)

"))

(th~defproblem _ISNTTIRP_THM (in landau3)
 (conclusion conc _ISNTTIRP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINE)

"))

(th~defproblem _ISRPNTT1_THM (in landau3)
 (conclusion conc _ISRPNTT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINOUT)

"))

(th~defproblem _ISNTTRP1_THM (in landau3)
 (conclusion conc _ISNTTRP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTIN _NT_NATRPI)

"))

(th~defproblem _ISNTENTT_THM (in landau3)
 (conclusion conc _ISNTENTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTERP _NATRPI _ISRPENTT)

"))

(th~defproblem _ISNTINTT_THM (in landau3)
 (conclusion conc _ISNTINTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NATRPI _ISRPINTT _ISNTIRP)

"))

(th~defproblem _ISNTTENT_THM (in landau3)
 (conclusion conc _ISNTTENT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTTERP _NT_NATRPI _ISRPENT)

"))

(th~defproblem _ISNTTINT_THM (in landau3)
 (conclusion conc _ISNTTINT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRPI _ISRPINT _ISNTTIRP)

"))

(th~defproblem _III5_T5_THM (in landau3)
 (conclusion conc _III5_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NATRPI _ISRPNTT1)

"))

(th~defproblem _III5_T6_THM (in landau3)
 (conclusion conc _III5_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T5 _NT_NATRPI _NATRPI _ISRPENT)

"))

(th~defproblem _ISNTNTT1_THM (in landau3)
 (conclusion conc _ISNTNTT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T6 _ISNTRP1 _NATRPI _TRIS)

"))

(th~defproblem _III5_T7_THM (in landau3)
 (conclusion conc _III5_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRPI _ISRPNT1)

"))

(th~defproblem _III5_T8_THM (in landau3)
 (conclusion conc _III5_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T7 _NATRPI _NT_NATRPI _ISRPENTT)

"))

(th~defproblem _ISNTTNT1_THM (in landau3)
 (conclusion conc _ISNTTNT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T8 _ISNTTRP1 _NT_NATRPI _TRIS)

"))

(th~defproblem _ISNTNTT2_THM (in landau3)
 (conclusion conc _ISNTNTT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTNTT1 _SYMIS)

"))

(th~defproblem _ISNTTNT2_THM (in landau3)
 (conclusion conc _ISNTTNT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTTNT1 _SYMIS)

"))

(th~defproblem _5156_T1_THM (in landau3)
 (conclusion conc _5156_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTINTT)

"))

(th~defproblem _SATZ156A_THM (in landau3)
 (conclusion conc _SATZ156A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5156_T1 _TH3)

"))

(th~defproblem _5156_T2_THM (in landau3)
 (conclusion conc _5156_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTINTT)

"))

(th~defproblem _SATZ156B_THM (in landau3)
 (conclusion conc _SATZ156B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5156_T2 _ISNTTINT)

"))

(th~defproblem _5156_T3_THM (in landau3)
 (conclusion conc _5156_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _5156_T4_THM (in landau3)
 (conclusion conc _5156_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTNTT2 _5156_T3 _ISP)

"))

(th~defproblem _5156_T5_THM (in landau3)
 (conclusion conc _5156_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5156_T4 _INDUCTION)

"))

(th~defproblem _SATZ156C_THM (in landau3)
 (conclusion conc _SATZ156C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTTNT2 _5156_T5 _ISP)

"))

(th~defproblem _NT_AX3T_THM (in landau3)
 (conclusion conc _NT_AX3T)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ156A)

"))

(th~defproblem _NT_AX4T_THM (in landau3)
 (conclusion conc _NT_AX4T)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ156B)

"))

(th~defproblem _NT_AX5T_THM (in landau3)
 (conclusion conc _NT_AX5T)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ156C)

"))

(th~defproblem _RTT_RATRPI_THM (in landau3)
 (conclusion conc _RTT_RATRPI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INP)

"))

(th~defproblem _ISRPERTT_THM (in landau3)
 (conclusion conc _ISRPERTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTI)

"))

(th~defproblem _ISRPIRTT_THM (in landau3)
 (conclusion conc _ISRPIRTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTE)

"))

(th~defproblem _ISRTTERP_THM (in landau3)
 (conclusion conc _ISRTTERP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINI)

"))

(th~defproblem _ISRTTIRP_THM (in landau3)
 (conclusion conc _ISRTTIRP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINE)

"))

(th~defproblem _ISRPRTT1_THM (in landau3)
 (conclusion conc _ISRPRTT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINOUT)

"))

(th~defproblem _ISRTTRP1_THM (in landau3)
 (conclusion conc _ISRTTRP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTIN _RTT_RATRPI)

"))

(th~defproblem _ISRTERTT_THM (in landau3)
 (conclusion conc _ISRTERTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISRTERP _RATRPI _ISRPERTT)

"))

(th~defproblem _ISRTIRTT_THM (in landau3)
 (conclusion conc _ISRTIRTT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RATRPI _ISRPIRTT _ISRTIRP)

"))

(th~defproblem _ISRTTERT_THM (in landau3)
 (conclusion conc _ISRTTERT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISRTTERP _RTT_RATRPI _ISRPERT)

"))

(th~defproblem _ISRTTIRT_THM (in landau3)
 (conclusion conc _ISRTTIRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RTT_RATRPI _ISRPIRT _ISRTTIRP)

"))

(th~defproblem _III5_T9_THM (in landau3)
 (conclusion conc _III5_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RATRPI _ISRPRTT1)

"))

(th~defproblem _III5_T10_THM (in landau3)
 (conclusion conc _III5_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T9 _RTT_RATRPI _RATRPI _ISRPERT)

"))

(th~defproblem _ISRTRTT1_THM (in landau3)
 (conclusion conc _ISRTRTT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T10 _ISRTRP1 _RATRPI _TRIS)

"))

(th~defproblem _III5_T11_THM (in landau3)
 (conclusion conc _III5_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RTT_RATRPI _ISRPRT1)

"))

(th~defproblem _III5_T12_THM (in landau3)
 (conclusion conc _III5_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T11 _RATRPI _RTT_RATRPI _ISRPERTT)

"))

(th~defproblem _ISRTTRT1_THM (in landau3)
 (conclusion conc _ISRTTRT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_III5_T12 _ISRTTRP1 _RTT_RATRPI _TRIS)

"))

(th~defproblem _EXAMPLE2_THM (in landau3)
 (conclusion conc _EXAMPLE2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ153C)

"))

(th~defproblem _5157_T1_THM (in landau3)
 (conclusion conc _5157_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _5157_T2_THM (in landau3)
 (conclusion conc _5157_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ82 _LRTRPOFRT)

"))

(th~defproblem _5157_T3_THM (in landau3)
 (conclusion conc _5157_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISRPRT2 _5157_T2 _ISP)

"))

(th~defproblem _5157_T4_THM (in landau3)
 (conclusion conc _5157_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T3 _5157_T1 _TH3)

"))

(th~defproblem _5157_T5_THM (in landau3)
 (conclusion conc _5157_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T4 _SATZ81E)

"))

(th~defproblem _5157_T6_THM (in landau3)
 (conclusion conc _5157_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T5)

"))

(th~defproblem _5157_T7_THM (in landau3)
 (conclusion conc _5157_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RT_MOREISI2 _URTRPOFRT)

"))

(th~defproblem _5157_T8_THM (in landau3)
 (conclusion conc _5157_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISRPRT2 _5157_T7 _ISP)

"))

(th~defproblem _5157_T9_THM (in landau3)
 (conclusion conc _5157_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T8 _ESTII)

"))

(th~defproblem _5157_T10_THM (in landau3)
 (conclusion conc _5157_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T9 _5157_T6 _ANDI)

"))

(th~defproblem _SATZ157A_THM (in landau3)
 (conclusion conc _SATZ157A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T10)

"))

(th~defproblem _SATZ157B_THM (in landau3)
 (conclusion conc _SATZ157B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T10 _SOMEI)

"))

(th~defproblem _5157_T11_THM (in landau3)
 (conclusion conc _5157_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _5157_T12_THM (in landau3)
 (conclusion conc _5157_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _5157_T13_THM (in landau3)
 (conclusion conc _5157_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T12 _ESTIE)

"))

(th~defproblem _5157_T14_THM (in landau3)
 (conclusion conc _5157_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T13 _CUTAPP2A)

"))

(th~defproblem _5157_T15_THM (in landau3)
 (conclusion conc _5157_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T14 _LRTRPOFRT)

"))

(th~defproblem _5157_T17_THM (in landau3)
 (conclusion conc _5157_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTII)

"))

(th~defproblem _5157_T18_THM (in landau3)
 (conclusion conc _5157_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T17 _5157_T11 _SATZ85)

"))

(th~defproblem _5157_T19_THM (in landau3)
 (conclusion conc _5157_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T18 _URTRPOFRT)

"))

(th~defproblem _5157_T20_THM (in landau3)
 (conclusion conc _5157_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T19 _CP)

"))

(th~defproblem _SATZ157C_THM (in landau3)
 (conclusion conc _SATZ157C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T20 _5157_T15 _ISI1)

"))

(th~defproblem _5157_T21_THM (in landau3)
 (conclusion conc _5157_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ157C _SOMEI)

"))

(th~defproblem _SATZ157D_THM (in landau3)
 (conclusion conc _SATZ157D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5157_T21 _SOMEAPP)

"))

(th~defproblem _5158_T1_THM (in landau3)
 (conclusion conc _5158_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RT_MOREISI2 _URTRPOFRT)

"))

(th~defproblem _5158_T2_THM (in landau3)
 (conclusion conc _5158_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T1 _ANDI)

"))

(th~defproblem _SATZ158A_THM (in landau3)
 (conclusion conc _SATZ158A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T2 _SOMEI)

"))

(th~defproblem _5158_T3_THM (in landau3)
 (conclusion conc _5158_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ157C _SYMIS)

"))

(th~defproblem _5158_T4_THM (in landau3)
 (conclusion conc _5158_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T3 _RP_MOREISI2)

"))

(th~defproblem _5158_T5_THM (in landau3)
 (conclusion conc _5158_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTII)

"))

(th~defproblem _5158_T6_THM (in landau3)
 (conclusion conc _5158_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T5 _AND_TH4)

"))

(th~defproblem _5158_T7_THM (in landau3)
 (conclusion conc _5158_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T6 _SOME_TH1)

"))

(th~defproblem _5158_T8_THM (in landau3)
 (conclusion conc _5158_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TH5)

"))

(th~defproblem _5158_T9_THM (in landau3)
 (conclusion conc _5158_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T8 _ESTIE)

"))

(th~defproblem _5158_T10_THM (in landau3)
 (conclusion conc _5158_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TH6)

"))

(th~defproblem _5158_T11_THM (in landau3)
 (conclusion conc _5158_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T10 _SATZ81K _SATZ82)

"))

(th~defproblem _5158_T12_THM (in landau3)
 (conclusion conc _5158_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T11 _LRTRPOFRT)

"))

(th~defproblem _5158_T13_THM (in landau3)
 (conclusion conc _5158_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T9 _5158_T12 _ANDI)

"))

(th~defproblem _5158_T14_THM (in landau3)
 (conclusion conc _5158_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T13 _SOMEI)

"))

(th~defproblem _5158_T15_THM (in landau3)
 (conclusion conc _5158_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T14 _5158_T7 _SOMEAPP)

"))

(th~defproblem _5158_T16_THM (in landau3)
 (conclusion conc _5158_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T15 _RP_MOREISI1)

"))

(th~defproblem _SATZ158B_THM (in landau3)
 (conclusion conc _SATZ158B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T16 _5158_T4 _TH1)

"))

(th~defproblem _5158_T17_THM (in landau3)
 (conclusion conc _5158_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123H)

"))

(th~defproblem _5158_T18_THM (in landau3)
 (conclusion conc _5158_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ158B _5158_T17 _TH3)

"))

(th~defproblem _SATZ158C_THM (in landau3)
 (conclusion conc _SATZ158C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5158_T18 _ET)

"))

(th~defproblem _5158_T19_THM (in landau3)
 (conclusion conc _5158_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123C)

"))

(th~defproblem _SATZ158D_THM (in landau3)
 (conclusion conc _SATZ158D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ158A _5158_T19 _TH3)

"))

(th~defproblem _5159_T1_THM (in landau3)
 (conclusion conc _5159_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154C _SATZ158B _SATZ124 _SATZ127A)

"))

(th~defproblem _5159_T2_THM (in landau3)
 (conclusion conc _5159_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ158A _5159_T1 _ANDI)

"))

(th~defproblem _5159_T3_THM (in landau3)
 (conclusion conc _5159_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5159_T2 _SOMEI)

"))

(th~defproblem _5159_T4_THM (in landau3)
 (conclusion conc _5159_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5159_T3 _CUTAPP3)

"))

(th~defproblem _SATZ159_THM (in landau3)
 (conclusion conc _SATZ159)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5159_T4 _LESSAPP)

"))

(th~defproblem _5159_T5_THM (in landau3)
 (conclusion conc _5159_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RATRPI _ANDI)

"))

(th~defproblem _5159_T6_THM (in landau3)
 (conclusion conc _5159_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5159_T5 _SOMEI)

"))

(th~defproblem _SATZ159A_THM (in landau3)
 (conclusion conc _SATZ159A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5159_T6 _SATZ159 _SOMEAPP)

"))

(th~defproblem _5159_T7_THM (in landau3)
 (conclusion conc _5159_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _5159_T8_THM (in landau3)
 (conclusion conc _5159_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _5159_T9_THM (in landau3)
 (conclusion conc _5159_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5159_T8 _5159_T7)

"))

(th~defproblem _SATZ159APP_THM (in landau3)
 (conclusion conc _SATZ159APP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5159_T9 _SATZ159 _SOMEAPP)

"))

(th~defproblem _5160_T1_THM (in landau3)
 (conclusion conc _5160_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITET)

"))

(th~defproblem _5160_T2_THM (in landau3)
 (conclusion conc _5160_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T1 _RP_LESSISI2)

"))

(th~defproblem _5160_T3_THM (in landau3)
 (conclusion conc _5160_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T2 _SATZ127A _RP_LESSISI1)

"))

(th~defproblem _5160_T4_THM (in landau3)
 (conclusion conc _5160_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITEF)

"))

(th~defproblem _5160_T5_THM (in landau3)
 (conclusion conc _5160_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T4 _RP_LESSISI2)

"))

(th~defproblem _5160_T6_THM (in landau3)
 (conclusion conc _5160_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123F _SATZ124 _5160_T5 _RP_TRLESSIS)

"))

(th~defproblem _5160_T7_THM (in landau3)
 (conclusion conc _5160_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T5 _5160_T3 _TH1)

"))

(th~defproblem _5160_T8_THM (in landau3)
 (conclusion conc _5160_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T6 _5160_T2 _TH1)

"))

(th~defproblem _5160_T9_THM (in landau3)
 (conclusion conc _5160_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ147A _RP_DISTTP2 _RP_ISLESS2)

"))

(th~defproblem _5160_T10_THM (in landau3)
 (conclusion conc _5160_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMTS _RP_ISPL2 _RP_DISTTP1 _TRIS _RP_LESSISI2)

"))

(th~defproblem _5160_T11_THM (in landau3)
 (conclusion conc _5160_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RP_LESSISI2 _5160_T7 _SATZ139A _SATZ149A)

"))

(th~defproblem _5160_T12_THM (in landau3)
 (conclusion conc _5160_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T11 _5160_T10 _SATZ139A)

"))

(th~defproblem _5160_T13_THM (in landau3)
 (conclusion conc _5160_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T12 _5160_T9 _SATZ127B)

"))

(th~defproblem _5160_T14_THM (in landau3)
 (conclusion conc _5160_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMPL _RP_ISPL1 _RP_ASSPL2 _TRIS)

"))

(th~defproblem _5160_T15_THM (in landau3)
 (conclusion conc _5160_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T14 _RP_ISTS1 _RP_DISTPT1 _TRIS)

"))

(th~defproblem _5160_T16_THM (in landau3)
 (conclusion conc _5160_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T15 _RP_ISPL2 _RP_ASSPL1 _TRIS)

"))

(th~defproblem _5160_T17_THM (in landau3)
 (conclusion conc _5160_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T13 _5160_T16 _RP_ISLESS2)

"))

(th~defproblem _5160_T18_THM (in landau3)
 (conclusion conc _5160_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _RP_LESSISI2 _5160_T8 _SATZ149A _SATZ153E _RP_COMTS _RP_ISLESSIS12)

"))

(th~defproblem _5160_T19_THM (in landau3)
 (conclusion conc _5160_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T18 _REFIS _RP_LESSISI2 _SATZ139A)

"))

(th~defproblem _5160_T20_THM (in landau3)
 (conclusion conc _5160_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T19 _5160_T17 _SATZ127B)

"))

(th~defproblem _5160_T21_THM (in landau3)
 (conclusion conc _5160_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T20 _SATZ140C _RP_ISLESS2)

"))

(th~defproblem _5160_T22_THM (in landau3)
 (conclusion conc _5160_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T21 _SATZ155C _SYMIS _RP_ISLESS1 _SATZ154F)

"))

(th~defproblem _5160_T23_THM (in landau3)
 (conclusion conc _5160_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110E)

"))

(th~defproblem _5160_T24_THM (in landau3)
 (conclusion conc _5160_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T22 _SATZ83 _5160_T23 _SYMIS _RT_ISMORE1)

"))

(th~defproblem _5160_T25_THM (in landau3)
 (conclusion conc _5160_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T24 _SATZ106A)

"))

(th~defproblem _5160_T26_THM (in landau3)
 (conclusion conc _5160_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122 _5160_T25 _SATZ154A _RP_TRMORE)

"))

(th~defproblem _5160_T27_THM (in landau3)
 (conclusion conc _5160_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122)

"))

(th~defproblem _5160_T28_THM (in landau3)
 (conclusion conc _5160_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T23 _5160_T27 _5160_T26 _AND3I)

"))

(th~defproblem _5160_T29_THM (in landau3)
 (conclusion conc _5160_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T28 _SOMEI)

"))

(th~defproblem _5160_T30_THM (in landau3)
 (conclusion conc _5160_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T29 _SOMEI)

"))

(th~defproblem _5160_T31_THM (in landau3)
 (conclusion conc _5160_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T30 _SATZ133A _SATZ159APP)

"))

(th~defproblem _SATZ160_THM (in landau3)
 (conclusion conc _SATZ160)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T31 _SATZ133A _SATZ159APP)

"))

(th~defproblem _5160_T32_THM (in landau3)
 (conclusion conc _5160_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E1)

"))

(th~defproblem _5160_T33_THM (in landau3)
 (conclusion conc _5160_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E2)

"))

(th~defproblem _5160_T34_THM (in landau3)
 (conclusion conc _5160_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E3)

"))

(th~defproblem _5160_T35_THM (in landau3)
 (conclusion conc _5160_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T34 _5160_T33 _5160_T32)

"))

(th~defproblem _5160_T36_THM (in landau3)
 (conclusion conc _5160_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T35 _SOMEAPP)

"))

(th~defproblem _SATZ160APP_THM (in landau3)
 (conclusion conc _SATZ160APP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5160_T36 _SATZ160 _SOMEAPP)

"))

(th~defproblem _5161_T1_THM (in landau3)
 (conclusion conc _5161_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ158A)

"))

(th~defproblem _5161_T2_THM (in landau3)
 (conclusion conc _5161_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T1 _ITET _RP_ISLESS2)

"))

(th~defproblem _5161_T3_THM (in landau3)
 (conclusion conc _5161_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T2 _RP_TRLESS)

"))

(th~defproblem _5161_T4_THM (in landau3)
 (conclusion conc _5161_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T1 _ITEF _RP_ISLESS2)

"))

(th~defproblem _5161_T5_THM (in landau3)
 (conclusion conc _5161_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123F _SATZ124 _5161_T4 _SATZ127B)

"))

(th~defproblem _5161_T6_THM (in landau3)
 (conclusion conc _5161_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T5 _5161_T2 _TH1)

"))

(th~defproblem _5161_T7_THM (in landau3)
 (conclusion conc _5161_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T4 _5161_T3 _TH1)

"))

(th~defproblem _5161_T8_THM (in landau3)
 (conclusion conc _5161_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ158B)

"))

(th~defproblem _5161_T9_THM (in landau3)
 (conclusion conc _5161_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T8 _ITET _RP_ISMOREIS2)

"))

(th~defproblem _5161_T10_THM (in landau3)
 (conclusion conc _5161_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_MOREISI1 _5161_T9 _RP_TRMOREIS)

"))

(th~defproblem _5161_T11_THM (in landau3)
 (conclusion conc _5161_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T8 _ITEF _RP_ISMOREIS2)

"))

(th~defproblem _5161_T12_THM (in landau3)
 (conclusion conc _5161_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ123E _SATZ125 _5161_T11 _RP_TRMOREIS)

"))

(th~defproblem _5161_T13_THM (in landau3)
 (conclusion conc _5161_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T12 _5161_T9 _TH1)

"))

(th~defproblem _5161_T14_THM (in landau3)
 (conclusion conc _5161_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T11 _5161_T10 _TH1)

"))

(th~defproblem _5161_T15_THM (in landau3)
 (conclusion conc _5161_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ147)

"))

(th~defproblem _5161_T16_THM (in landau3)
 (conclusion conc _5161_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T15 _SATZ123B _EC3E21)

"))

(th~defproblem _5161_T17_THM (in landau3)
 (conclusion conc _5161_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TRIS2)

"))

(th~defproblem _5161_T18_THM (in landau3)
 (conclusion conc _5161_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T17 _5161_T16)

"))

(th~defproblem _5161_T19_THM (in landau3)
 (conclusion conc _5161_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T17 _SYMIS _SATZ122 _5161_T16)

"))

(th~defproblem _5161_T20_THM (in landau3)
 (conclusion conc _5161_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T19 _5161_T18 _SATZ123A _OR3E1)

"))

(th~defproblem _5161_T21_THM (in landau3)
 (conclusion conc _5161_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T20)

"))

(th~defproblem _5161_T22_THM (in landau3)
 (conclusion conc _5161_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T6)

"))

(th~defproblem _5161_T23_THM (in landau3)
 (conclusion conc _5161_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T7)

"))

(th~defproblem _5161_T24_THM (in landau3)
 (conclusion conc _5161_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T23 _SATZ151A _RP_ISLESS1)

"))

(th~defproblem _5161_T25_THM (in landau3)
 (conclusion conc _5161_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T24 _5161_T22 _REFIS _RP_LESSISI2 _SATZ148C _RP_TRLESS)

"))

(th~defproblem _5161_T26_THM (in landau3)
 (conclusion conc _5161_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T25 _ESTII)

"))

(th~defproblem _5161_T27_THM (in landau3)
 (conclusion conc _5161_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T13)

"))

(th~defproblem _5161_T28_THM (in landau3)
 (conclusion conc _5161_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T14)

"))

(th~defproblem _5161_T29_THM (in landau3)
 (conclusion conc _5161_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T28 _SATZ151A _RP_ISMOREIS1)

"))

(th~defproblem _5161_T30_THM (in landau3)
 (conclusion conc _5161_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T29 _5161_T27 _REFIS _RP_MOREISI2 _SATZ149 _RP_TRMOREIS)

"))

(th~defproblem _5161_T31_THM (in landau3)
 (conclusion conc _5161_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T30 _SATZ123C)

"))

(th~defproblem _5161_T32_THM (in landau3)
 (conclusion conc _5161_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE _5161_T31 _TH3)

"))

(th~defproblem _5161_T33_THM (in landau3)
 (conclusion conc _5161_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTIE)

"))

(th~defproblem _5161_T34_THM (in landau3)
 (conclusion conc _5161_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154C)

"))

(th~defproblem _5161_T35_THM (in landau3)
 (conclusion conc _5161_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T33 _5161_T34 _SATZ147A _RP_TRLESS)

"))

(th~defproblem _5161_T36_THM (in landau3)
 (conclusion conc _5161_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T35 _ESTII)

"))

(th~defproblem _5161_T37_THM (in landau3)
 (conclusion conc _5161_T37)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T33 _SATZ122)

"))

(th~defproblem _5161_T38_THM (in landau3)
 (conclusion conc _5161_T38)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T6)

"))

(th~defproblem _5161_T39_THM (in landau3)
 (conclusion conc _5161_T39)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T7)

"))

(th~defproblem _5161_T40_THM (in landau3)
 (conclusion conc _5161_T40)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ94)

"))

(th~defproblem _5161_T41_THM (in landau3)
 (conclusion conc _5161_T41)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_DISTTP2 _SATZ155A _RP_ISTS12 _TRIS)

"))

(th~defproblem _5161_T42_THM (in landau3)
 (conclusion conc _5161_T42)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T41 _SYMIS)

"))

(th~defproblem _5161_T43_THM (in landau3)
 (conclusion conc _5161_T43)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_COMTS _RP_ISPL2 _RP_DISTTP1 _TRIS _RP_LESSISI2)

"))

(th~defproblem _5161_T44_THM (in landau3)
 (conclusion conc _5161_T44)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T38 _REFIS _RP_LESSISI2 _SATZ138C _SATZ145C)

"))

(th~defproblem _5161_T45_THM (in landau3)
 (conclusion conc _5161_T45)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T44 _5161_T43 _SATZ138C)

"))

(th~defproblem _5161_T46_THM (in landau3)
 (conclusion conc _5161_T46)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RP_DISTPT1 _RP_ISPL2 _RP_ASSPL1 _TRIS)

"))

(th~defproblem _5161_T47_THM (in landau3)
 (conclusion conc _5161_T47)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T45 _5161_T46 _5161_T42 _RP_ISLESS12)

"))

(th~defproblem _5161_T48_THM (in landau3)
 (conclusion conc _5161_T48)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T39 _REFIS _RP_LESSISI2 _SATZ148C _SATZ153C _RP_ISLESS2)

"))

(th~defproblem _5161_T49_THM (in landau3)
 (conclusion conc _5161_T49)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T48 _REFIS _RP_LESSISI2 _SATZ138C)

"))

(th~defproblem _5161_T50_THM (in landau3)
 (conclusion conc _5161_T50)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T49 _5161_T37 _SATZ140C _RP_ISLESS2)

"))

(th~defproblem _5161_T51_THM (in landau3)
 (conclusion conc _5161_T51)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T50 _5161_T47 _RP_TRLESS)

"))

(th~defproblem _5161_T52_THM (in landau3)
 (conclusion conc _5161_T52)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T51 _ESTII)

"))

(th~defproblem _5161_T53_THM (in landau3)
 (conclusion conc _5161_T53)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T40 _5161_T52 _ANDI)

"))

(th~defproblem _5161_T54_THM (in landau3)
 (conclusion conc _5161_T54)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T53 _SOMEI)

"))

(th~defproblem _5161_T55_THM (in landau3)
 (conclusion conc _5161_T55)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T54 _CUTAPP1A)

"))

(th~defproblem _5161_T56_THM (in landau3)
 (conclusion conc _5161_T56)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T55 _5161_T36 _5161_T32 _5161_T26 _CUT2)

"))

(th~defproblem _5161_T57_THM (in landau3)
 (conclusion conc _5161_T57)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T56 _CUTAPP1B)

"))

(th~defproblem _5161_T58_THM (in landau3)
 (conclusion conc _5161_T58)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T57 _CUTAPP1A)

"))

(th~defproblem _5161_T59_THM (in landau3)
 (conclusion conc _5161_T59)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ154B _SATZ154C _TH9)

"))

(th~defproblem _5161_T60_THM (in landau3)
 (conclusion conc _5161_T60)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ84 _5161_T59 _SATZ125)

"))

(th~defproblem _5161_T61_THM (in landau3)
 (conclusion conc _5161_T61)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ121)

"))

(th~defproblem _5161_T62_THM (in landau3)
 (conclusion conc _5161_T62)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ158C)

"))

(th~defproblem _5161_T63_THM (in landau3)
 (conclusion conc _5161_T63)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITET _SYMIS)

"))

(th~defproblem _5161_T64_THM (in landau3)
 (conclusion conc _5161_T64)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T63 _ISP)

"))

(th~defproblem _5161_T65_THM (in landau3)
 (conclusion conc _5161_T65)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T63 _RT_LESSISI2)

"))

(th~defproblem _5161_T66_THM (in landau3)
 (conclusion conc _5161_T66)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T65 _SATZ82 _SATZ87B _RT_LESSISI1)

"))

(th~defproblem _5161_T67_THM (in landau3)
 (conclusion conc _5161_T67)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITEF _SYMIS)

"))

(th~defproblem _5161_T68_THM (in landau3)
 (conclusion conc _5161_T68)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T67 _ISP)

"))

(th~defproblem _5161_T69_THM (in landau3)
 (conclusion conc _5161_T69)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T67 _RT_LESSISI2)

"))

(th~defproblem _5161_T70_THM (in landau3)
 (conclusion conc _5161_T70)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T69 _SATZ81E _SATZ88)

"))

(th~defproblem _5161_T71_THM (in landau3)
 (conclusion conc _5161_T71)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T68 _5161_T64 _TH1)

"))

(th~defproblem _5161_T72_THM (in landau3)
 (conclusion conc _5161_T72)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T70 _5161_T65 _TH1)

"))

(th~defproblem _5161_T73_THM (in landau3)
 (conclusion conc _5161_T73)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T69 _5161_T66 _TH1)

"))

(th~defproblem _5161_T74_THM (in landau3)
 (conclusion conc _5161_T74)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T71 _5161_T58 _INI)

"))

(th~defproblem _5161_T75_THM (in landau3)
 (conclusion conc _5161_T75)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T72 _5161_T59)

"))

(th~defproblem _5161_T76_THM (in landau3)
 (conclusion conc _5161_T76)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T73 _5161_T59)

"))

(th~defproblem _T77_THM (in landau3)
 (conclusion conc _T77)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ155C _SATZ154B _TRIS)

"))

(th~defproblem _T78_THM (in landau3)
 (conclusion conc _T78)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T76 _5161_T75 _SATZ149A _T77 _SYMIS _RP_ISLESSIS1)

"))

(th~defproblem _T79_THM (in landau3)
 (conclusion conc _T79)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T74 _ESTIE)

"))

(th~defproblem _T80_THM (in landau3)
 (conclusion conc _T80)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T79 _T78 _SATZ127A)

"))

(th~defproblem _T81_THM (in landau3)
 (conclusion conc _T81)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T80 _SATZ122 _SATZ123B _EC3E23)

"))

(th~defproblem _T82_THM (in landau3)
 (conclusion conc _T82)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T81 _5161_T62 _TSAPP)

"))

(th~defproblem _T82A_THM (in landau3)
 (conclusion conc _T82A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T82 _5161_T62 _TSAPP)

"))

(th~defproblem _T83_THM (in landau3)
 (conclusion conc _T83)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T82A _5161_T61 _SATZ159APP)

"))

(th~defproblem _T84_THM (in landau3)
 (conclusion conc _T84)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ122)

"))

(th~defproblem _T85_THM (in landau3)
 (conclusion conc _T85)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITET _SYMIS)

"))

(th~defproblem _T86_THM (in landau3)
 (conclusion conc _T86)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T85 _SATZ154B)

"))

(th~defproblem _T87_THM (in landau3)
 (conclusion conc _T87)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T86 _RP_ISMORE1)

"))

(th~defproblem _T88_THM (in landau3)
 (conclusion conc _T88)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T86 _RP_MOREISI2)

"))

(th~defproblem _T89_THM (in landau3)
 (conclusion conc _T89)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T88 _SATZ154C _SATZ122 _SATZ127D _RP_MOREISI1)

"))

(th~defproblem _T90_THM (in landau3)
 (conclusion conc _T90)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ITEF _SYMIS)

"))

(th~defproblem _T91_THM (in landau3)
 (conclusion conc _T91)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T90 _SATZ154B)

"))

(th~defproblem _T92_THM (in landau3)
 (conclusion conc _T92)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T91 _RP_ISMORE1)

"))

(th~defproblem _T93_THM (in landau3)
 (conclusion conc _T93)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T91 _RP_MOREISI2)

"))

(th~defproblem _T94_THM (in landau3)
 (conclusion conc _T94)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T93 _SATZ81F _5161_T60 _RP_TRMOREIS)

"))

(th~defproblem _T95_THM (in landau3)
 (conclusion conc _T95)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T92 _T87 _TH1)

"))

(th~defproblem _T96_THM (in landau3)
 (conclusion conc _T96)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T94 _T88 _TH1)

"))

(th~defproblem _T97_THM (in landau3)
 (conclusion conc _T97)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T93 _T89 _TH1)

"))

(th~defproblem _T98_THM (in landau3)
 (conclusion conc _T98)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T95 _RP_MOREISI1 _SATZ158D)

"))

(th~defproblem _T99_THM (in landau3)
 (conclusion conc _T99)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5161_T58 _INE _T98 _TH3)

"))

(th~defproblem _T100_THM (in landau3)
 (conclusion conc _T100)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ESTII _T99 _TH3)

"))

(th~defproblem _T101_THM (in landau3)
 (conclusion conc _T101)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T100 _SATZ123F)

"))

(th~defproblem _T101A_THM (in landau3)
 (conclusion conc _T101A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T97 _T96 _SATZ149)

"))

(th~defproblem _T102_THM (in landau3)
 (conclusion conc _T102)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T101A _SATZ154B _SATZ155C _SYMIS _TRIS _RP_ISMOREIS1)

"))

(th~defproblem _T103_THM (in landau3)
 (conclusion conc _T103)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T101 _T102 _RP_TRMOREIS)

"))

(th~defproblem _T104_THM (in landau3)
 (conclusion conc _T104)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T103 _SATZ123C)

"))

(th~defproblem _T105_THM (in landau3)
 (conclusion conc _T105)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T104 _T84 _SATZ160APP)

"))

(th~defproblem _T106_THM (in landau3)
 (conclusion conc _T106)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T105 _SATZ159APP)

"))

(th~defproblem _T107_THM (in landau3)
 (conclusion conc _T107)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T106 _T83 _SATZ123A _OR3E1)

"))

(th~defproblem _T108_THM (in landau3)
 (conclusion conc _T108)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T107 _SOMEI)

"))

(th~defproblem _SATZ161_THM (in landau3)
 (conclusion conc _SATZ161)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_T108 _5161_T21 _ONEI)

"))

(th~defproblem _5162_T1_THM (in landau3)
 (conclusion conc _5162_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ28H _SATZ28G _ISPL1 _TRIS)

"))

(th~defproblem _5162_T2_THM (in landau3)
 (conclusion conc _5162_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ18A _5162_T1 _ISLESS2)

"))

(th~defproblem _5162_T3_THM (in landau3)
 (conclusion conc _5162_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ36 _SATZ10H _TH3 _SATZ10J)

"))

(th~defproblem _5162_T4_THM (in landau3)
 (conclusion conc _5162_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _ISPL2 _DISTTP1 _TRIS)

"))

(th~defproblem _5162_T5_THM (in landau3)
 (conclusion conc _5162_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSPL2 _DISTTP1 _5162_T4 _ISPL12 _DISTTP2 _TR3IS)

"))

(th~defproblem _5162_T6_THM (in landau3)
 (conclusion conc _5162_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_5162_T1 _ISPL2 _ASSPL1 _TRIS)

"))

(th~defproblem _NUN_THM (in landau3)
 (conclusion conc _NUN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_5162_T6 _ISPL1 _5162_T5 _TRIS)

"))

(th~defproblem _NUN1_THM (in landau3)
 (conclusion conc _NUN1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_NUN _SYMIS)

"))

(th~defproblem _5162_T7_THM (in landau3)
 (conclusion conc _5162_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ27A)

"))

(th~defproblem _5162_T8_THM (in landau3)
 (conclusion conc _5162_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T7 _ANDE1)

"))

(th~defproblem _5162_T9_THM (in landau3)
 (conclusion conc _5162_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T7 _ANDE2)

"))

(th~defproblem _5162_T10_THM (in landau3)
 (conclusion conc _5162_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TFEQ12A _TREQ1)

"))

(th~defproblem _5162_T11_THM (in landau3)
 (conclusion conc _5162_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ28A _NDIS12 _5162_T10 _12ISND _TR4IS)

"))

(th~defproblem _5162_T12_THM (in landau3)
 (conclusion conc _5162_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T2 _5162_T11 _ISLESS2)

"))

(th~defproblem _5162_T13_THM (in landau3)
 (conclusion conc _5162_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T2 _REFIS _LESSISI2 _SATZ35C _5162_T11 _ASSTS1 _TRIS _ISLESS1)

"))

(th~defproblem _5162_T14_THM (in landau3)
 (conclusion conc _5162_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T12 _5162_T3)

"))

(th~defproblem _5162_T15_THM (in landau3)
 (conclusion conc _5162_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T13 _5162_T3)

"))

(th~defproblem _5162_T16_THM (in landau3)
 (conclusion conc _5162_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T15 _5162_T1 _SYMIS _ISLESS12)

"))

(th~defproblem _5162_T17_THM (in landau3)
 (conclusion conc _5162_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T16 _SATZ20F)

"))

(th~defproblem _5162_T18_THM (in landau3)
 (conclusion conc _5162_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS)

"))

(th~defproblem _5162_T19_THM (in landau3)
 (conclusion conc _5162_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NUN _ISTS12 _TRIS)

"))

(th~defproblem _5162_T20_THM (in landau3)
 (conclusion conc _5162_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ASSPL1 _5162_T19 _ISPL1 _TRIS)

"))

(th~defproblem _5162_T21_THM (in landau3)
 (conclusion conc _5162_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_DISTTP2 _N_ISTS2 _COMTS _TR3IS)

"))

(th~defproblem _5162_T22_THM (in landau3)
 (conclusion conc _5162_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_DISTTP2 _5162_T21 _N_ISTS2 _TRIS)

"))

(th~defproblem _5162_T23_THM (in landau3)
 (conclusion conc _5162_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ASSPL2 _5162_T22 _ISPL2 _TRIS)

"))

(th~defproblem _5162_T24_THM (in landau3)
 (conclusion conc _5162_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ASSPL1 _5162_T23 _ISPL1 _5162_T20 _TR3IS)

"))

(th~defproblem _5162_T25_THM (in landau3)
 (conclusion conc _5162_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T18 _ISTS12 _NUN1 _COMPL _ISPL1 _ASSPL2 _TR4IS)

"))

(th~defproblem _5162_T26_THM (in landau3)
 (conclusion conc _5162_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ASSPL2 _COMPL _5162_T25 _ISPL2 _5162_T24 _TR4IS)

"))

(th~defproblem _5162_T27_THM (in landau3)
 (conclusion conc _5162_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T11 _5162_T1 _TRIS)

"))

(th~defproblem _5162_T28_THM (in landau3)
 (conclusion conc _5162_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T27 _ISPL1 _5162_T26 _TRIS)

"))

(th~defproblem _5162_T29_THM (in landau3)
 (conclusion conc _5162_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T28 _SATZ20E)

"))

(th~defproblem _5162_T30_THM (in landau3)
 (conclusion conc _5162_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_12ISND _SATZ28E _5162_T29 _SYMIS _NDIS12 _TR4IS)

"))

(th~defproblem _5162_T31_THM (in landau3)
 (conclusion conc _5162_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T30 _TFEQ12A _TREQ2)

"))

(th~defproblem _5162_T32_THM (in landau3)
 (conclusion conc _5162_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T31 _SOMEI)

"))

(th~defproblem _5162_T33_THM (in landau3)
 (conclusion conc _5162_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T32 _5162_T8)

"))

(th~defproblem _5162_T34_THM (in landau3)
 (conclusion conc _5162_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T33 _5162_T17 _SATZ12 _SATZ10G)

"))

(th~defproblem _5162_T35_THM (in landau3)
 (conclusion conc _5162_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T34 _5162_T17 _SOMEAPP)

"))

(th~defproblem _5162_T36_THM (in landau3)
 (conclusion conc _5162_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T35 _5162_T14 _SOMEAPP)

"))

(th~defproblem _5162_T37_THM (in landau3)
 (conclusion conc _5162_T37)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T36 _5162_T9 _SOMEAPP)

"))

(th~defproblem _5162_T38_THM (in landau3)
 (conclusion conc _5162_T38)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASS _TICT _ISE)

"))

(th~defproblem _5162_T39_THM (in landau3)
 (conclusion conc _5162_T39)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_FRIS _REFEQ1)

"))

(th~defproblem _5162_T40_THM (in landau3)
 (conclusion conc _5162_T40)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T39 _EQTF12)

"))

(th~defproblem _5162_T41_THM (in landau3)
 (conclusion conc _5162_T41)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T38 _5162_T40 _TREQ)

"))

(th~defproblem _5162_T42_THM (in landau3)
 (conclusion conc _5162_T42)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T41 _SOMEI)

"))

(th~defproblem _5162_T43_THM (in landau3)
 (conclusion conc _5162_T43)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T42 _SOMEI)

"))

(th~defproblem _5162_T44_THM (in landau3)
 (conclusion conc _5162_T44)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T43 _5162_T37)

"))

(th~defproblem _5162_T45_THM (in landau3)
 (conclusion conc _5162_T45)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T44 _RATAPP1)

"))

(th~defproblem _5162_T46_THM (in landau3)
 (conclusion conc _5162_T46)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ161)

"))

(th~defproblem _5162_T47_THM (in landau3)
 (conclusion conc _5162_T47)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T46 _ISRPRT2 _RP_ISTS12 _SATZ155C _TR3IS)

"))

(th~defproblem _5162_T48_THM (in landau3)
 (conclusion conc _5162_T48)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T47 _ISRTIRP)

"))

(th~defproblem _5162_T49_THM (in landau3)
 (conclusion conc _5162_T49)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T48 _5162_T45)

"))

(th~defproblem _SATZ162_THM (in landau3)
 (conclusion conc _SATZ162)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5162_T49 _SOMEI)

"))

(th~defproblem _THSQRT1_THM (in landau3)
 (conclusion conc _THSQRT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ161)

"))

(th~defproblem _THSQRT2_THM (in landau3)
 (conclusion conc _THSQRT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_THSQRT1 _5161_T20)

"))

(th~defproblem _ISSQRT_THM (in landau3)
 (conclusion conc _ISSQRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _IIIA_T1_THM (in landau3)
 (conclusion conc _IIIA_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISRPNT1)

"))

(th~defproblem _IIIA_T2_THM (in landau3)
 (conclusion conc _IIIA_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISRPNT1)

"))

(th~defproblem _IIIA_T3_THM (in landau3)
 (conclusion conc _IIIA_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T2 _IIIA_T1 _RP_ISPL12)

"))

(th~defproblem _IIIA_T4_THM (in landau3)
 (conclusion conc _IIIA_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NATRTI)

"))

(th~defproblem _IIIA_T5_THM (in landau3)
 (conclusion conc _IIIA_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NATRTI)

"))

(th~defproblem _IIIA_T6_THM (in landau3)
 (conclusion conc _IIIA_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ155A _SYMIS)

"))

(th~defproblem _IIIA_T7_THM (in landau3)
 (conclusion conc _IIIA_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T5 _IIIA_T4 _SATZ112D)

"))

(th~defproblem _IIIA_T8_THM (in landau3)
 (conclusion conc _IIIA_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T7 _ISRTN1)

"))

(th~defproblem _IIIA_T9_THM (in landau3)
 (conclusion conc _IIIA_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T8 _ISRTERP)

"))

(th~defproblem _IIIA_T10_THM (in landau3)
 (conclusion conc _IIIA_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T9 _IIIA_T6 _IIIA_T3 _TR3IS)

"))

(th~defproblem _NATPL_THM (in landau3)
 (conclusion conc _NATPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T10 _SOMEI)

"))

(th~defproblem _IIIA_T11_THM (in landau3)
 (conclusion conc _IIIA_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T2 _IIIA_T1 _RP_ISTS12)

"))

(th~defproblem _IIIA_T12_THM (in landau3)
 (conclusion conc _IIIA_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ155C _SYMIS)

"))

(th~defproblem _IIIA_T13_THM (in landau3)
 (conclusion conc _IIIA_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T5 _IIIA_T4 _SATZ112F)

"))

(th~defproblem _IIIA_T14_THM (in landau3)
 (conclusion conc _IIIA_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T13 _ISRTN1)

"))

(th~defproblem _IIIA_T15_THM (in landau3)
 (conclusion conc _IIIA_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T14 _ISRTERP)

"))

(th~defproblem _IIIA_T16_THM (in landau3)
 (conclusion conc _IIIA_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T15 _IIIA_T12 _IIIA_T11 _TR3IS)

"))

(th~defproblem _NATTS_THM (in landau3)
 (conclusion conc _NATTS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T16 _SOMEI)

"))

(th~defproblem _IIIA_T17_THM (in landau3)
 (conclusion conc _IIIA_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T2 _IIIA_T1 _RP_ISMORE12)

"))

(th~defproblem _IIIA_T18_THM (in landau3)
 (conclusion conc _IIIA_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T17 _SATZ154D)

"))

(th~defproblem _IIIA_T20_THM (in landau3)
 (conclusion conc _IIIA_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T2 _IIIA_T1 _IIIA_T18 _SATZ154A _RP_ISMN12)

"))

(th~defproblem _IIIA_T21_THM (in landau3)
 (conclusion conc _IIIA_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T18 _SATZ155B _SATZ154A _SYMIS)

"))

(th~defproblem _IIIA_T22_THM (in landau3)
 (conclusion conc _IIIA_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T18 _IIIA_T5 _IIIA_T4 _SATZ112G)

"))

(th~defproblem _IIIA_T23_THM (in landau3)
 (conclusion conc _IIIA_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T22 _IIIA_T18 _ISRTN1)

"))

(th~defproblem _IIIA_T24_THM (in landau3)
 (conclusion conc _IIIA_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T23 _IIIA_T18 _ISRTERP)

"))

(th~defproblem _IIIA_T25_THM (in landau3)
 (conclusion conc _IIIA_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T24 _IIIA_T21 _IIIA_T20 _IIIA_T18 _SATZ154A _TR3IS)

"))

(th~defproblem _NATMN_THM (in landau3)
 (conclusion conc _NATMN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_IIIA_T25 _SOMEI)

"))

(th~defproblem _CLCL1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _CLCL _AND3E1) _CLCL1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CLCL2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _CLCL _AND3E2) _CLCL2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CLCL3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _CLCL _AND3E3) _CLCL3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CLCL1A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _CLCL1 _ANDE1) _CLCL1A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CLCL1B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _CLCL1 _ANDE2) _CLCL1B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _CLCL2 _MP) _III1_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CUTAPP2A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T2 _MP) _CUTAPP2A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CUTAPP2B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _CUTAPP2A _SATZ83) _CUTAPP2B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T3 _AND_TH4) _III1_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES _TH5 _III1_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _TH6 _III1_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T7 _SATZ81J) _III1_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T8 _III1_T6) _III1_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CUT1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _III1_T11 _III1_T10) _AND3I) _CUT1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _1119_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ81B _EC3E23) _1119_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ119_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _CUTAPP2A _1119_T1) _TH3) _SATZ119))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ119A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ83 _SATZ119) _SATZ119A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _1120_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ81B _EC3E32) _1120_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ120_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _CUTAPP2B _1120_T1) _TH7) _SATZ120))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ120A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ82 _SATZ120) _SATZ120A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T12 _TH3) _III1_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T13 _SATZ81F) _III1_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T16_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T15 _TH3) _III1_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _III1_T16 _III1_T14) _ORE1) _III1_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T17 _SATZ82) _III1_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES _III1_T18 _III1_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _III1_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _III1_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T22 _SATZ81G) _III1_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T24_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ84 _III1_T23) _TH3) _III1_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T25_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _III1_T24 _III1_T21) _TH4) _III1_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T27_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T26 _AND_TH1) _III1_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T29_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND_TH2 _III1_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T30_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _III1_T29 _III1_T28) _TH1) _III1_T30))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III1_T31_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III1_T30 _III1_T11) _III1_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _CUT2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _III1_T31 _III1_T19) _III1_T10) _AND3I) _CUT2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III2_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _III2_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III2_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _III2_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III2_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III2_T2 _III2_T1) _III2_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III2_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _III2_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III2_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _III2_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III2_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III2_T5 _III2_T4) _III2_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2121_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDI _2121_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ121_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2121_T2 _MOREAPP) _SATZ121))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2122_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDI _2122_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ122_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2122_T2 _LESSAPP) _SATZ122))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_ISE _2123_T1) _TH3) _2123_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T2 _MOREAPP) _2123_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T3 _EC_TH2) _2123_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_ISE _2123_T5) _TH3) _2123_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T6 _LESSAPP) _2123_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T7 _EC_TH1) _2123_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2A _2123_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2B _2123_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _2123_T10 _SATZ81B) _EC3E23) _2123_T9) _MP) _2123_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T11 _LESSAPP) _2123_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T12 _MOREAPP) _2123_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES _2123_T13 _2123_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T15_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T14 _EC_TH1) _2123_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _2123_T8 _2123_T15) _2123_T4) _EC3_TH6) _2123_A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T16_HTHM (in landau3)
 (conclusion conc (IMPLIES _OR3I1 _2123_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RP_ISI _TH3) _2123_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ121 _2123_T18) _TH8) _2123_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2123_T19 _OR3_TH7) _2123_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2123_B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _2123_T20 _2123_T16) _TH1) _2123_B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _2123_A _2123_B) _OREC3I) _SATZ123))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ123 _OREC3E1) _SATZ123A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ123 _OREC3E2) _SATZ123B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_MOREISI2_HTHM (in landau3)
 (conclusion conc (IMPLIES _ORI2 _RP_MOREISI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_LESSISI2_HTHM (in landau3)
 (conclusion conc (IMPLIES _ORI2 _RP_LESSISI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_MOREISI1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ORI1 _RP_MOREISI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_LESSISI1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ORI1 _RP_LESSISI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_ISMORE12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RP_ISMORE1 _RP_ISMORE2) _RP_ISMORE12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_ISLESS12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RP_ISLESS1 _RP_ISLESS2) _RP_ISLESS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_ISMOREIS12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RP_ISMOREIS1 _RP_ISMOREIS2) _RP_ISMOREIS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_ISLESSIS12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RP_ISLESSIS1 _RP_ISLESSIS2) _RP_ISLESSIS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _COMOR _SATZ123B) _EC3_TH7) _SATZ123C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ123B _EC3_TH9) _SATZ123D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123E_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ123A _OR3_TH2) _SATZ123E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123F_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ123A _OR3_TH3) _COMOR) _SATZ123F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123G_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ123B _EC3E21) _EC3E23) _OR_TH3) _SATZ123G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123H_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ123B _EC3E31) _EC3E32) _OR_TH3) _SATZ123H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123J_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH4 _OR_TH5) _SATZ123A) _OR3E3) _SATZ123J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ123K_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH5 _OR_TH4) _SATZ123A) _OR3E2) _SATZ123K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2126_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2A _2126_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2126_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2126_T1 _SATZ119A) _2126_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2126_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2126_T2 _ANDI) _2126_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2126_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2126_T4 _LESSAPP) _2126_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ126_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _2126_T5 _LESSAPP) _SATZ126))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_TRLESS_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ126 _RP_TRLESS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_TRMORE_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ121 _SATZ126) _SATZ122) _RP_TRMORE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ127B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_ISLESS2 _RP_TRLESS) _ORAPP) _SATZ127B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ127C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ124 _SATZ121) _SATZ127B) _SATZ122) _SATZ127C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ127D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ121 _SATZ124) _SATZ127A) _SATZ122) _SATZ127D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2128_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ127A _RP_LESSISI1) _2128_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2128_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _2128_T1 _2128_T2) _ORAPP) _2128_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _2128_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ127B _RP_LESSISI1) _2128_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ128_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _2128_T3 _2128_T4) _ORAPP) _SATZ128))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_TRLESSIS_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ128 _RP_TRLESSIS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_TRMOREIS_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ124 _SATZ128) _SATZ125) _RP_TRMOREIS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3I _III3_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E1 _III3_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E2 _III3_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E3 _III3_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _III3_T7 _III3_T6) _III3_T5) _III3_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2A _3129_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2A _3129_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3129_T3 _SATZ81B) _EC3E31) _3129_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T4 _SUMAPP) _3129_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES _RT_ISLESS2 _3129_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3129_T6 _EXAMPLE1D) _SATZ110F) _RT_ISLESS12) _3129_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T7 _SATZ106C) _3129_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3129_T8 _SATZ105F) _EXAMPLE1A) _RT_ISLESS2) _3129_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3129_T8 _SATZ105F) _EXAMPLE1A) _RT_ISLESS2) _3129_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T9 _SATZ120) _3129_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T10 _SATZ120) _3129_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T15_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3129_T14 _3129_T12) _3129_T11) _SUM1) _3129_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T16_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T15 _SUMAPP) _3129_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ83 _SATZ96A) _3129_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3129_T19 _3129_T17) _ANDI) _3129_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T21 _CUTAPP3) _3129_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T22 _SUMAPP) _3129_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T25_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T24 _CUTAPP1B) _3129_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T26_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T25 _CUTAPP1B) _3129_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3129_T27_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T26 _CUTAPP1A) _3129_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ129_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3129_T27 _CUTAPP1A) _SATZ129))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LRTPL_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SUM1 _SATZ129) _INE) _LRTPL))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _URTPL_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ129 _INI) _III3_T10) _TH3) _URTPL))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ129 _INI) _III3_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PLAPP_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III3_T11 _SUMAPP) _PLAPP))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3130_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3130_T1 _LRTPL) _3130_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3130_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3130_T2 _PLAPP) _3130_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ130_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3130_T3 _ISI1) _SATZ130))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_COMPL_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ130 _RP_COMPL))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3131_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3131_T1 _3131_T2) _LRTPL) _3131_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3131_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3131_T3 _PLAPP) _3131_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3131_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3131_T4 _PLAPP) _3131_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3131_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3131_T6 _3131_T7) _LRTPL) _3131_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3131_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3131_T8 _PLAPP) _3131_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3131_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3131_T9 _PLAPP) _3131_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ131_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3131_T10 _3131_T5) _ISI1) _SATZ131))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_ASSPL1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ131 _RP_ASSPL1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _ANDE2 _ANDE1) _CUTAPP2B) _3132_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2B _3132_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3132_T2 _SATZ96D) _3132_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3132_T3 _3132_T2) _SATZ101C) _RT_ISMORE2) _3132_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3132_T4 _SATZ119) _3132_T2) _3132_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _3132_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _3132_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3132_T11 _ANDI) _3132_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3132_T13 _3132_T12) _ANDI) _3132_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ111D _INCLASS) _MOREI) _3132_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3132_T17 _NATRTI) _SATZ112G) _3132_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ94A _3132_T17) _SATZ101E) _RT_ISLESS2) _3132_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _3132_T19 _INCLASS) _3132_T18) _INCLASSN) _LESSE) _3132_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3132_T20 _SATZ111C) _3132_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ14 _3132_T21) _SATZ10H) _TH3) _3132_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3132_T8 _3132_T22) _TH3) _ET) _3132_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T25_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3132_T9 _3132_T24) _ANDI) _3132_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T29_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3132_T28 _3132_T25) _ANDI) _3132_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T32_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3132_T16 _3132_T31) _SATZ24) _ORAPP) _3132_T32))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T35_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3132_T34 _CUTAPP1B) _3132_T35))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ132_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _3132_T35 _CUTAPP1A) _ANDE2) _ANDE1) _CUTAPP2B) _SATZ132))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T36_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _ANDE1 _CUTAPP2B) _3132_T36))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T37_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _R_ANDE2 _CUTAPP2B) _3132_T36) _3132_T37))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T38_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3132_T36 _ANDE1) _CUTAPP2B) _3132_T38))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T39_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3132_T36 _ANDE2) _CUTAPP2B) _3132_T39))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T40_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND (AND _3132_T39 _3132_T38) _CUTAPP2B) _SATZ101C) _3132_T36) _3132_T1) _SATZ101G) _3132_T40))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3132_T42_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3132_T41 _3132_T39) _3132_T38) _CUTAPP2B) _3132_T42))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3133_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3133_T1 _LRTPL) _CUTAPP2B) _3133_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3133_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3133_T2 _ANDI) _CUTAPP2B) _3133_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3133_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3133_T4 _CUTAPP2B) _SATZ132APP) _3133_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ133_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3133_T5 _CUTAPP1A) _SATZ133))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ133A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ133 _SATZ121) _SATZ133A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3134_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ119A _3134_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3134_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ83 _3134_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3134_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3134_T4 _LRTPL) _3134_T2) _CUTAPP2B) _3134_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3134_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _3134_T6 _3134_T5) _ANDI) _3134_T2) _CUTAPP2B) _3134_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3134_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3134_T8 _3134_T2) _CUTAPP2B) _SATZ132APP) _3134_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3134_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3134_T9 _CUTAPP3) _3134_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ134_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3134_T10 _MOREAPP) _SATZ134))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135A_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ134 _SATZ135A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135B_HTHM (in landau3)
 (conclusion conc (IMPLIES _RP_ISPL1 _SATZ135B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ122 _SATZ134) _SATZ121) _SATZ135C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135A _RP_COMPL) _RP_ISMORE12) _SATZ135D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135E_HTHM (in landau3)
 (conclusion conc (IMPLIES _RP_ISPL2 _SATZ135E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135F_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135C _RP_COMPL) _RP_ISLESS12) _SATZ135F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135G_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135D _RP_ISPL1) _RP_ISMORE2) _SATZ135G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135H_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135G _RP_COMPL) _RP_ISMORE12) _SATZ135H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135J_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135F _RP_ISPL1) _RP_ISLESS2) _SATZ135J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ135K_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135J _RP_COMPL) _RP_ISLESS12) _SATZ135K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3136_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ123A _3136_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3136_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ123B _3136_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ136A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ135C _SATZ135A) _SATZ135B) _3136_T2) _3136_T1) _EC3_TH11) _SATZ136A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ136B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ135C _SATZ135A) _SATZ135B) _3136_T2) _3136_T1) _EC3_TH10) _SATZ136B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ136C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ135C _SATZ135A) _SATZ135B) _3136_T2) _3136_T1) _EC3_TH12) _SATZ136C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ136D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_COMPL _RP_ISMORE12) _SATZ136A) _SATZ136D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ136F_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_COMPL _RP_ISLESS12) _SATZ136C) _SATZ136F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3137_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ134 _3137_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3137_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ134 _RP_COMPL) _RP_ISMORE12) _3137_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ137_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3137_T2 _3137_T1) _RP_TRMORE) _SATZ137))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ137A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ122 _SATZ137) _SATZ121) _SATZ137A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ138A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135G _SATZ137) _ORAPP) _SATZ138A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ138B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ135H _SATZ137) _ORAPP) _SATZ138B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ138C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ122 _SATZ125) _SATZ138A) _SATZ121) _SATZ138C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ138D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ125 _SATZ122) _SATZ138B) _SATZ121) _SATZ138D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3139_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RP_ISPL12 _RP_MOREISI2) _3139_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3139_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ138A _RP_MOREISI1) _3139_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3139_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3139_T1 _3139_T2) _ORAPP) _3139_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3139_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ138B _RP_MOREISI1) _3139_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ139_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3139_T3 _3139_T4) _ORAPP) _SATZ139))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ139A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ125 _SATZ139) _SATZ124) _SATZ139A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ133 _RP_ISMORE1) _3140_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3140_T1 _SATZ123D) _TH3) _3140_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ135D _3140_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3140_T3 _SATZ123B) _EC3E21) _3140_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ135F _3140_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3140_T5 _SATZ123B) _EC3E31) _3140_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ123A _OR3_TH1) _3140_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3140_T6 _3140_T4) _3140_T7) _ORAPP) _3140_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T11A _ANDI) _III3_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III3_T12 _AND3I) _III3_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E1 _III3_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E2 _III3_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E3 _III3_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III3_T19 _ANDE1) _III3_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _III3_T19 _R_ANDE2) _III3_T20) _III3_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III3_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _III3_T21 _III3_T20) _III3_T18) _III3_T17) _III3_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES _3134_T2 _3140_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _CUTAPP2A _3140_T11) _RT_TRLESS) _3140_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3140_T12 _SATZ81B) _EC3E31) _3140_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T13 _DIFFAPP) _3140_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ96C _3140_T16) _RT_ISLESS2) _3140_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T17 _SATZ120) _3140_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ94 _RT_COMPL) _RT_ISMORE1) _3140_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RT_COMPL _3140_T19) _SATZ101G) _3140_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3140_T20 _3140_T19) _3140_T18) _DIFF1) _3140_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T21 _DIFFAPP) _3140_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ83 _3140_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T24_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T23 _RT_TRMORE) _3140_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T25_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3140_T23 _SATZ101F) _3140_T24) _RT_ISMORE12) _3140_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T26_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3140_T25 _3140_T24) _SATZ97A) _3140_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T29_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3140_T27 _3140_T28) _3140_T24) _ANDI) _3140_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T31_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T30 _CUTAPP3) _3140_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T32_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T31 _DIFFAPP) _3140_T32))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T33_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _3140_T32 _3140_T22) _3140_T15) _3140_T10) _3140_T9) _CUT2) _3140_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T34_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T33 _CUTAPP1B) _3140_T34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T35_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T34 _CUTAPP3) _3140_T35))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ140H_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T35 _MOREAPP) _SATZ140H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T36_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2B _3140_T36))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T38_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3140_T36 _SATZ94A) _3140_T37) _RT_ISLESS2) _3140_T38))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T41_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T40 _SATZ120) _3140_T41))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T42_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _3140_T41 _SATZ140H) _INI) _DIFFAPP) _3140_T42))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _3140_T42 _PLAPP) _3140_A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T43_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ83 _3140_T43))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T44_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2B _3140_T44))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T45_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _CUTAPP2B _3140_T43) _3140_T44) _3140_T45))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_T48_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _3140_T45 _SATZ94) _3140_T47) _RT_ISMORE1) _3140_T43) _3140_T44) _3140_T48))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T49_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _3140_T47 _3140_T48) _3140_T45) _SATZ101G) _3140_T43) _3140_T44) _T49))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T53_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _T52 _3140_T43) _CUTAPP2B) _SATZ132APP) _T53))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T54_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T53 _CUTAPP3) _T54))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T55_HTHM (in landau3)
 (conclusion conc (IMPLIES _T54 _T55))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T56_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _CUTAPP2A _T55) _SATZ120) _T56))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T57_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T56 _MOREAPP) _T57))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _3140_B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T54 _T57) _TH1) _3140_B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T58_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _3140_B _3140_A) _ISI1) _T58))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T59_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ140B _T59))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ140C_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ140 _SATZ140C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ140G_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ140C _SATZ140B) _SATZ140G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_ISMN12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T60 _SATZ140G) _RP_ISMN12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III4_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3I _III4_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III4_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E1 _III4_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III4_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E2 _III4_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III4_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E3 _III4_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III4_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _III4_T7 _III4_T6) _III4_T5) _III4_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2A _4141_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2A _4141_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4141_T3 _SATZ81B) _EC3E31) _4141_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T4 _PRODAPP) _4141_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ141B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T6 _SATZ110G) _SATZ141B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _RT_ISLESS2 _4141_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _4141_T7 _SATZ105F) _4141_T8) _SATZ141B) _RT_ISLESS12) _4141_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T9 _SATZ120) _4141_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ110D _4141_T10) _PROD1) _4141_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T11 _PRODAPP) _4141_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ83 _SATZ105A) _4141_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T16_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4141_T15 _4141_T13) _ANDI) _4141_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T17 _CUTAPP3) _4141_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T18 _PRODAPP) _4141_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T20 _CUTAPP1B) _4141_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T21 _CUTAPP1B) _4141_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4141_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T22 _CUTAPP1A) _4141_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ141_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4141_T23 _CUTAPP1A) _SATZ141))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LRTTS_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _PROD1 _SATZ141) _INE) _LRTTS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _URTTS_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ141 _INI) _III4_T10) _TH3) _URTTS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III4_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ141 _INI) _III4_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TSAPP_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _III4_T11 _PRODAPP) _TSAPP))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4142_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4142_T1 _LRTTS) _4142_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4142_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4142_T2 _TSAPP) _4142_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ142_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4142_T3 _ISI1) _SATZ142))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_COMTS_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ142 _RP_COMTS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4143_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4143_T1 _4143_T2) _LRTTS) _4143_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4143_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4143_T3 _TSAPP) _4143_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4143_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4143_T4 _TSAPP) _4143_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4143_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4143_T6 _4143_T7) _LRTTS) _4143_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4143_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4143_T8 _TSAPP) _4143_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4143_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4143_T9 _TSAPP) _4143_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ143_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4143_T10 _4143_T5) _ISI1) _SATZ143))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_ASSTS1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ143 _RP_ASSTS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4144_T1 _4144_T3) _4144_T2) _LRTPL) _4144_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4144_T4 _PLAPP) _4144_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4144_T5 _TSAPP) _4144_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4144_T10 _SATZ84) _SATZ88) _4144_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T15_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4144_T14 _SATZ81J) _SATZ87B) _RT_LESSISI1) _4144_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T16_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4144_T13 _4144_T9) _TH1) _4144_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4144_T15 _4144_T10) _TH1) _4144_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4144_T14 _4144_T11) _TH1) _4144_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T25_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4144_T24 _TSAPP) _4144_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T26_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4144_T25 _TSAPP) _4144_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4144_T27_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4144_T26 _PLAPP) _4144_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ144_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4144_T27 _4144_T6) _ISI1) _SATZ144))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RP_DISTTP2_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ144 _RP_DISTTP2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4145_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ140D _4145_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145B_HTHM (in landau3)
 (conclusion conc (IMPLIES _RP_ISTS1 _SATZ145B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ122 _SATZ145A) _SATZ121) _SATZ145C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145A _RP_COMTS) _RP_ISMORE12) _SATZ145D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145E_HTHM (in landau3)
 (conclusion conc (IMPLIES _RP_ISTS2 _SATZ145E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145F_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145C _RP_COMTS) _RP_ISLESS12) _SATZ145F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145G_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145D _RP_ISTS1) _RP_ISMORE2) _SATZ145G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145H_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145G _RP_COMTS) _RP_ISMORE12) _SATZ145H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145J_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145F _RP_ISTS1) _RP_ISLESS2) _SATZ145J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ145K_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145J _RP_COMTS) _RP_ISLESS12) _SATZ145K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4146_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ123A _4146_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4146_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ123B _4146_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ146A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ145C _SATZ145A) _SATZ145B) _4146_T2) _4146_T1) _EC3_TH11) _SATZ146A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ146B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ145C _SATZ145A) _SATZ145B) _4146_T2) _4146_T1) _EC3_TH10) _SATZ146B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ146C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ145C _SATZ145A) _SATZ145B) _4146_T2) _4146_T1) _EC3_TH12) _SATZ146C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ146D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_COMTS _RP_ISMORE12) _SATZ146A) _SATZ146D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ146F_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_COMTS _RP_ISLESS12) _SATZ146C) _SATZ146F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4147_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ145A _4147_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4147_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145A _RP_COMTS) _RP_ISMORE12) _4147_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ147_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4147_T2 _4147_T1) _RP_TRMORE) _SATZ147))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ147A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ122 _SATZ147) _SATZ121) _SATZ147A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ148A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145G _SATZ147) _ORAPP) _SATZ148A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ148B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ145H _SATZ147) _ORAPP) _SATZ148B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ148C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ122 _SATZ125) _SATZ148A) _SATZ121) _SATZ148C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ148D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ125 _SATZ122) _SATZ148B) _SATZ121) _SATZ148D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4149_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RP_ISTS12 _RP_MOREISI2) _4149_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4149_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ148A _RP_MOREISI1) _4149_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4149_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4149_T1 _4149_T2) _ORAPP) _4149_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4149_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ148B _RP_MOREISI1) _4149_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ149_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4149_T3 _4149_T4) _ORAPP) _SATZ149))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ149A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ125 _SATZ149) _SATZ124) _SATZ149A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4150_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ90 _4150_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4150_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4150_T5 _RT_TRLESS) _4150_T2) _4150_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4150_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4150_T5 _SATZ91) _4150_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4150_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _4150_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4150_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _4150_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4150_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _4150_T8 _SATZ83) _4150_T9) _4150_T2) _ANDI) _4150_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4150_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _4150_T11 _4150_T6) _4150_T4) _4150_T2) _CUT2) _4150_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LRTRPOFRT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4150_T2 _SATZ150) _INE) _LRTRPOFRT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LRTRPOFRTE_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ150 _INI) _4150_T5) _LRTRPOFRTE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III4_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ81C _III4_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _URTRPOFRT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _LRTRPOFRTE _III4_T12) _TH3) _URTRPOFRT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4151_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _4151_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4151_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4151_T2 _SATZ120) _4151_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4151_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4151_T3 _TSAPP) _4151_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4151_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ105F _SATZ110E) _RT_ISLESS2) _4151_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4151_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4151_T5 _LRTRPOFRT) _4151_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4151_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4151_T8 _CUTAPP3) _4151_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ151_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4151_T9 _4151_T4) _ISI1) _SATZ151))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDI _4152_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T2 _AND3I) _4152_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E1 _4152_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E2 _4152_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E3 _4152_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _4152_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _4152_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4152_T8 _4152_T10) _4152_T9) _4152_T6) _4152_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ94A _4152_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T13 _SATZ119A) _4152_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ110E _4152_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ110E _4152_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4152_T19 _4152_T17) _SATZ110B) _4152_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4152_T20 _4152_T16) _TH3) _4152_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES _4152_T22 _4152_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T24_HTHM (in landau3)
 (conclusion conc (IMPLIES _RT_ISLESS2 _4152_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T26_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4152_T24 _SATZ105C) _4152_T25) _RT_ISLESS2) _4152_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T27_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4152_T26 _RT_COMTS) _RT_ISLESS12) _4152_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T28_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T27 _SATZ106C) _4152_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T29_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T28 _SATZ119A) _4152_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T30_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ110E _4152_T30))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T31_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T30 _SATZ110G) _4152_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T32_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4152_T31 _4152_T28) _4152_T29) _INV1) _4152_T32))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T33_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T32 _INVAPP) _4152_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T34_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ91 _4152_T34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T35_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _4152_T35))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T36_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T35 _SATZ119A) _4152_T36))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T38_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _4152_T38))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T40_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4152_T38 _SATZ105C) _4152_T39) _RT_ISLESS2) _4152_T40))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T41_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4152_T40 _RT_COMTS) _RT_ISLESS12) _4152_T41))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T42_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T41 _SATZ106C) _4152_T42))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T44_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4152_T43 _4152_T37) _ANDI) _4152_T44))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T47_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T46 _INVAPP) _4152_T47))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T48_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _4152_T47 _4152_T33) _4152_T23) _4152_T15) _CUT2) _4152_T48))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T49_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T48 _CUTAPP1B) _4152_T49))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T50_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T49 _CUTAPP1A) _4152_T50))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T52_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2A _4152_T52))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T54_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T53 _LRTRPOFRT) _4152_T54))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _R1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T50 _INI) _R1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _R2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4152_T54 _R1) _INVAPP) _R2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _R3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _R2 _TSAPP) _R3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T55_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _4152_T55))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T56_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _4152_T55 _SATZ83) _4152_T56))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T57_HTHM (in landau3)
 (conclusion conc (IMPLIES _CUTAPP2B _4152_T57))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T58_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _CUTAPP2A _4152_T56) _4152_T57) _4152_T58))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4152_T59_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4152_T58 _4152_T56) _SATZ105F) _4152_T57) _4152_T59))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T62_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4152_T60 _4152_T56) _4152_T57) _SATZ96C) _T62))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T63_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _T62 _T61) _4152_T57) _4152_T56) _RT_ISLESS2) _T63))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T64_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _T63 _4152_T57) _RT_COMPL) _RT_ISLESS12) _4152_T56) _T64))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T65_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _T64 _4152_T57) _SATZ97C) _4152_T56) _T65))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T67_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND (AND _T65 _SATZ105F) _SATZ141B) _T66) _RT_ISLESS12) _4152_T56) _4152_T57) _T67))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T68_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _T67 _SATZ119A) _4152_T56) _4152_T57) _T68))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T69_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ110E _4152_T56) _4152_T57) _T69))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T72_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _T71 _4152_T50) _INE) _4152_T56) _4152_T57) _T72))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T73_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _T70 _T72) _LRTTS) _4152_T56) _4152_T57) _T73))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T74_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _T73 _4152_T56) _CUTAPP2B) _SATZ132APP) _T74))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T75_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T74 _CUTAPP1A) _T75))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T76_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T75 _R3) _ISI1) _T76))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4153_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ145D _4153_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4153_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4153_T1 _SATZ123B) _EC3E21) _4153_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4153_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ145F _4153_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4153_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _4153_T3 _SATZ123B) _EC3E31) _4153_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4153_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ123A _OR3_TH1) _4153_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4153_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _4153_T4 _4153_T2) _4153_T5) _ORAPP) _4153_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _4153_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ153B _4153_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ153C_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ153 _SATZ153C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ153G_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ153C _SATZ153B) _SATZ153G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5154_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ82 _LRTRPOFRT) _5154_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5154_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5154_T2 _5154_T1) _ANDI) _5154_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ154C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ83 _SATZ154A) _SATZ121) _SATZ154C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5154_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ81A _5154_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5154_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ123B _5154_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ154D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ154C _SATZ154A) _SATZ154B) _5154_T5) _5154_T4) _EC3_TH11) _SATZ154D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ154E_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ154C _SATZ154A) _SATZ154B) _5154_T5) _5154_T4) _EC3_TH10) _SATZ154E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ154F_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ154C _SATZ154A) _SATZ154B) _5154_T5) _5154_T4) _EC3_TH12) _SATZ154F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ154E _III5_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTERP_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ154B _ISRTERP))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTIRP_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ154E _ISRTIRP))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNTIRP_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _ISRTIRP _ISNIRT) _ISNTIRP))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES _ISNTIRP _III5_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _5155_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _5155_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5155_T2 _5155_T1) _SATZ98A) _5155_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T4 _LRTRPOFRT) _5155_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T5 _PLAPP) _5155_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _5155_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5155_T7 _EXAMPLE1D) _SATZ110F) _RT_ISLESS12) _5155_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T8 _SATZ106C) _5155_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ105C _EXAMPLE1C) _RT_COMTS) _RT_ISLESS12) _5155_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T11 _LRTRPOFRT) _5155_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5155_T10 _5155_T9) _5155_T12) _LRTPL) _5155_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ155A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5155_T6 _5155_T13) _ISI1) _SATZ155A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ101F _5155_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ155B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5155_T16 _SATZ154A) _SATZ140G) _SATZ155B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _5155_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _5155_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5155_T18 _5155_T17) _SATZ107A) _5155_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T20 _LRTRPOFRT) _5155_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T21 _TSAPP) _5155_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES _LRTRPOFRTE _5155_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T24_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _5155_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T25_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _5155_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T26_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5155_T24 _EXAMPLE1D) _SATZ110F) _RT_ISLESS12) _5155_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T27_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T26 _SATZ106C) _5155_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T28_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5155_T25 _SATZ110F) _RT_ISLESS1) _5155_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T29_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T28 _SATZ106C) _5155_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T31_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _5155_T30 _5155_T27) _5155_T12) _5155_T29) _LRTRPOFRT) _LRTTS) _5155_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ155C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5155_T22 _5155_T32) _ISI1) _SATZ155C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5155_T33_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ110F _5155_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ155D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5155_T35 _SATZ153G) _SATZ155D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNTENTT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _ISNTERP _NATRPI) _ISRPENTT) _ISNTENTT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNTINTT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _NATRPI _ISRPINTT) _ISNTIRP) _ISNTINTT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNTTENT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _ISNTTERP _NT_NATRPI) _ISRPENT) _ISNTTENT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNTTINT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _NT_NATRPI _ISRPINT) _ISNTTIRP) _ISNTTINT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _NATRPI _ISRPNTT1) _III5_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _III5_T5 _NT_NATRPI) _NATRPI) _ISRPENT) _III5_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _NT_NATRPI _ISRPNT1) _III5_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _III5_T7 _NATRPI) _NT_NATRPI) _ISRPENTT) _III5_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5156_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ISNTINTT _5156_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ156A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5156_T1 _TH3) _SATZ156A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5156_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _ISNTINTT _5156_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ156B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5156_T2 _ISNTTINT) _SATZ156B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5156_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5156_T4 _INDUCTION) _5156_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_AX3T_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ156A _NT_AX3T))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_AX4T_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ156B _NT_AX4T))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_AX5T_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ156C _NT_AX5T))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTERTT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _ISRTERP _RATRPI) _ISRPERTT) _ISRTERTT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTIRTT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RATRPI _ISRPIRTT) _ISRTIRP) _ISRTIRTT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTTERT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _ISRTTERP _RTT_RATRPI) _ISRPERT) _ISRTTERT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTTIRT_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RTT_RATRPI _ISRPIRT) _ISRTTIRP) _ISRTTIRT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RATRPI _ISRPRTT1) _III5_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _III5_T9 _RTT_RATRPI) _RATRPI) _ISRPERT) _III5_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RTT_RATRPI _ISRPRT1) _III5_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _III5_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _III5_T11 _RATRPI) _RTT_RATRPI) _ISRPERTT) _III5_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EXAMPLE2_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ153C _EXAMPLE2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _SATZ82 _LRTRPOFRT) _5157_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5157_T3 _5157_T1) _TH3) _5157_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5157_T4 _SATZ81E) _5157_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES _5157_T5 _5157_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5157_T9 _5157_T6) _ANDI) _5157_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ157A_HTHM (in landau3)
 (conclusion conc (IMPLIES _5157_T10 _SATZ157A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _5157_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _5157_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5157_T13 _CUTAPP2A) _5157_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T15_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5157_T14 _LRTRPOFRT) _5157_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5157_T17 _5157_T11) _SATZ85) _5157_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5157_T18 _URTRPOFRT) _5157_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5157_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5157_T19 _CP) _5157_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ157C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5157_T20 _5157_T15) _ISI1) _SATZ157C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5158_T1 _ANDI) _5158_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5158_T3 _RP_MOREISI2) _5158_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5158_T5 _AND_TH4) _5158_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES _TH5 _5158_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES _TH6 _5158_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5158_T10 _SATZ81K) _SATZ82) _5158_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5158_T11 _LRTRPOFRT) _5158_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5158_T9 _5158_T12) _ANDI) _5158_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T16_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5158_T15 _RP_MOREISI1) _5158_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ158B_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5158_T16 _5158_T4) _TH1) _SATZ158B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ123H _5158_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ158B _5158_T17) _TH3) _5158_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ158C_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5158_T18 _ET) _SATZ158C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5158_T19_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ123C _5158_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ158D_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ158A _5158_T19) _TH3) _SATZ158D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5159_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ154C _SATZ158B) _SATZ124) _SATZ127A) _5159_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5159_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ158A _5159_T1) _ANDI) _5159_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5159_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5159_T3 _CUTAPP3) _5159_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ159_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5159_T4 _LESSAPP) _SATZ159))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5159_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _RATRPI _ANDI) _5159_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5159_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE1 _5159_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5159_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES _ANDE2 _5159_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5159_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5159_T8 _5159_T7) _5159_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5160_T1 _RP_LESSISI2) _5160_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T2 _SATZ127A) _RP_LESSISI1) _5160_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5160_T4 _RP_LESSISI2) _5160_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ123F _SATZ124) _5160_T5) _RP_TRLESSIS) _5160_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T5 _5160_T3) _TH1) _5160_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T6 _5160_T2) _TH1) _5160_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ147A _RP_DISTTP2) _RP_ISLESS2) _5160_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T11 _5160_T10) _SATZ139A) _5160_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T12 _5160_T9) _SATZ127B) _5160_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T13 _5160_T16) _RP_ISLESS2) _5160_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T19 _5160_T17) _SATZ127B) _5160_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T20 _SATZ140C) _RP_ISLESS2) _5160_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ110E _5160_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T25_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5160_T24 _SATZ106A) _5160_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T26_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ122 _5160_T25) _SATZ154A) _RP_TRMORE) _5160_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T27_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ122 _5160_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T28_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5160_T23 _5160_T27) _5160_T26) _AND3I) _5160_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T31_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T30 _SATZ133A) _SATZ159APP) _5160_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ160_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T31 _SATZ133A) _SATZ159APP) _SATZ160))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T32_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E1 _5160_T32))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T33_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E2 _5160_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T34_HTHM (in landau3)
 (conclusion conc (IMPLIES _AND3E3 _5160_T34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5160_T35_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5160_T34 _5160_T33) _5160_T32) _5160_T35))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ158A _5161_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T2 _RP_TRLESS) _5161_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ123F _SATZ124) _5161_T4) _SATZ127B) _5161_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T6_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T5 _5161_T2) _TH1) _5161_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T4 _5161_T3) _TH1) _5161_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ158B _5161_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _RP_MOREISI1 _5161_T9) _RP_TRMOREIS) _5161_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ123E _SATZ125) _5161_T11) _RP_TRMOREIS) _5161_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T12 _5161_T9) _TH1) _5161_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T11 _5161_T10) _TH1) _5161_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T15_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ147 _5161_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T16_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T15 _SATZ123B) _EC3E21) _5161_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T17 _5161_T16) _5161_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5161_T19 _5161_T18) _SATZ123A) _OR3E1) _5161_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T21_HTHM (in landau3)
 (conclusion conc (IMPLIES _5161_T20 _5161_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES _5161_T6 _5161_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES _5161_T7 _5161_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T24_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T23 _SATZ151A) _RP_ISLESS1) _5161_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T27_HTHM (in landau3)
 (conclusion conc (IMPLIES _5161_T13 _5161_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T28_HTHM (in landau3)
 (conclusion conc (IMPLIES _5161_T14 _5161_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T29_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T28 _SATZ151A) _RP_ISMOREIS1) _5161_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T31_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T30 _SATZ123C) _5161_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T34_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ154C _5161_T34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T35_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5161_T33 _5161_T34) _SATZ147A) _RP_TRLESS) _5161_T35))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T37_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T33 _SATZ122) _5161_T37))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T38_HTHM (in landau3)
 (conclusion conc (IMPLIES _5161_T6 _5161_T38))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T39_HTHM (in landau3)
 (conclusion conc (IMPLIES _5161_T7 _5161_T39))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T40_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ94 _5161_T40))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T45_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T44 _5161_T43) _SATZ138C) _5161_T45))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T47_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5161_T45 _5161_T46) _5161_T42) _RP_ISLESS12) _5161_T47))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T50_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5161_T49 _5161_T37) _SATZ140C) _RP_ISLESS2) _5161_T50))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T51_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T50 _5161_T47) _RP_TRLESS) _5161_T51))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T53_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T40 _5161_T52) _ANDI) _5161_T53))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T55_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T54 _CUTAPP1A) _5161_T55))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T56_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _5161_T55 _5161_T36) _5161_T32) _5161_T26) _CUT2) _5161_T56))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T57_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T56 _CUTAPP1B) _5161_T57))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T58_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T57 _CUTAPP1A) _5161_T58))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T59_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ154B _SATZ154C) _TH9) _5161_T59))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T60_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ84 _5161_T59) _SATZ125) _5161_T60))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T61_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ121 _5161_T61))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T62_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ158C _5161_T62))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T65_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T63 _RT_LESSISI2) _5161_T65))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T66_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5161_T65 _SATZ82) _SATZ87B) _RT_LESSISI1) _5161_T66))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T69_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T67 _RT_LESSISI2) _5161_T69))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T70_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T69 _SATZ81E) _SATZ88) _5161_T70))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T71_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T68 _5161_T64) _TH1) _5161_T71))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T72_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T70 _5161_T65) _TH1) _5161_T72))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T73_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T69 _5161_T66) _TH1) _5161_T73))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T74_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5161_T71 _5161_T58) _INI) _5161_T74))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T75_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T72 _5161_T59) _5161_T75))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5161_T76_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5161_T73 _5161_T59) _5161_T76))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T80_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T79 _T78) _SATZ127A) _T80))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T81_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _T80 _SATZ122) _SATZ123B) _EC3E23) _T81))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T82_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T81 _5161_T62) _TSAPP) _T82))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T82A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T82 _5161_T62) _TSAPP) _T82A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T83_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T82A _5161_T61) _SATZ159APP) _T83))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T84_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ122 _T84))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T86_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T85 _SATZ154B) _T86))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T87_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T86 _RP_ISMORE1) _T87))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T88_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T86 _RP_MOREISI2) _T88))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T89_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _T88 _SATZ154C) _SATZ122) _SATZ127D) _RP_MOREISI1) _T89))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T91_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T90 _SATZ154B) _T91))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T92_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T91 _RP_ISMORE1) _T92))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T93_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T91 _RP_MOREISI2) _T93))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T94_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _T93 _SATZ81F) _5161_T60) _RP_TRMOREIS) _T94))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T95_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T92 _T87) _TH1) _T95))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T96_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T94 _T88) _TH1) _T96))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T97_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T93 _T89) _TH1) _T97))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T98_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T95 _RP_MOREISI1) _SATZ158D) _T98))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T99_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5161_T58 _INE) _T98) _TH3) _T99))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T101_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T100 _SATZ123F) _T101))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T101A_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T97 _T96) _SATZ149) _T101A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T103_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T101 _T102) _RP_TRMOREIS) _T103))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T104_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T103 _SATZ123C) _T104))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T105_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _T104 _T84) _SATZ160APP) _T105))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T106_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _T105 _SATZ159APP) _T106))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _T107_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _T106 _T83) _SATZ123A) _OR3E1) _T107))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _SATZ18A _5162_T1) _ISLESS2) _5162_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ36 _SATZ10H) _TH3) _SATZ10J) _5162_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ27A _5162_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T7 _ANDE1) _5162_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T7 _ANDE2) _5162_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T10_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _TFEQ12A _TREQ1) _5162_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T12_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5162_T2 _5162_T11) _ISLESS2) _5162_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T12 _5162_T3) _5162_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T15_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T13 _5162_T3) _5162_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T16 _SATZ20F) _5162_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T29_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T28 _SATZ20E) _5162_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T31_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5162_T30 _TFEQ12A) _TREQ2) _5162_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T33_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T32 _5162_T8) _5162_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T34_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _5162_T33 _5162_T17) _SATZ12) _SATZ10G) _5162_T34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T38_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _INCLASS _TICT) _ISE) _5162_T38))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T39_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _FRIS _REFEQ1) _5162_T39))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T40_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T39 _EQTF12) _5162_T40))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T41_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _5162_T38 _5162_T40) _TREQ) _5162_T41))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T44_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T43 _5162_T37) _5162_T44))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T45_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T44 _RATAPP1) _5162_T45))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T46_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ161 _5162_T46))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T48_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T47 _ISRTIRP) _5162_T48))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5162_T49_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _5162_T48 _5162_T45) _5162_T49))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _THSQRT1_HTHM (in landau3)
 (conclusion conc (IMPLIES _SATZ161 _THSQRT1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _THSQRT2_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _THSQRT1 _5161_T20) _THSQRT2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T1_HTHM (in landau3)
 (conclusion conc (IMPLIES _ISRPNT1 _IIIA_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T2_HTHM (in landau3)
 (conclusion conc (IMPLIES _ISRPNT1 _IIIA_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T3_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _IIIA_T2 _IIIA_T1) _RP_ISPL12) _IIIA_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T4_HTHM (in landau3)
 (conclusion conc (IMPLIES _NATRTI _IIIA_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T5_HTHM (in landau3)
 (conclusion conc (IMPLIES _NATRTI _IIIA_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T7_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _IIIA_T5 _IIIA_T4) _SATZ112D) _IIIA_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T8_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _IIIA_T7 _ISRTN1) _IIIA_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T9_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _IIIA_T8 _ISRTERP) _IIIA_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T11_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _IIIA_T2 _IIIA_T1) _RP_ISTS12) _IIIA_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T13_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _IIIA_T5 _IIIA_T4) _SATZ112F) _IIIA_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T14_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _IIIA_T13 _ISRTN1) _IIIA_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T15_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _IIIA_T14 _ISRTERP) _IIIA_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T17_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _IIIA_T2 _IIIA_T1) _RP_ISMORE12) _IIIA_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T18_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND _IIIA_T17 _SATZ154D) _IIIA_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T20_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _IIIA_T2 _IIIA_T1) _IIIA_T18) _SATZ154A) _RP_ISMN12) _IIIA_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T22_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND (AND _IIIA_T18 _IIIA_T5) _IIIA_T4) _SATZ112G) _IIIA_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T23_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _IIIA_T22 _IIIA_T18) _ISRTN1) _IIIA_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _IIIA_T24_HTHM (in landau3)
 (conclusion conc (IMPLIES (AND (AND _IIIA_T23 _IIIA_T18) _ISRTERP) _IIIA_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

