(th~defproblem _NUMIS_THM (in landau2)
 (conclusion conc _NUMIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_FIRST1IS1)

"))

(th~defproblem _ISNUM_THM (in landau2)
 (conclusion conc _ISNUM)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_FIRST1IS2)

"))

(th~defproblem _DENIS_THM (in landau2)
 (conclusion conc _DENIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SECOND1IS1)

"))

(th~defproblem _ISDEN_THM (in landau2)
 (conclusion conc _ISDEN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SECOND1IS2)

"))

(th~defproblem _FRIS_THM (in landau2)
 (conclusion conc _FRIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR1IS1)

"))

(th~defproblem _ISFR_THM (in landau2)
 (conclusion conc _ISFR)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PAIR1IS2)

"))

(th~defproblem _12ISND_THM (in landau2)
 (conclusion conc _12ISND)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_ISDEN _ISNUM _ISTS12)

"))

(th~defproblem _NDIS12_THM (in landau2)
 (conclusion conc _NDIS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_12ISND _SYMIS)

"))

(th~defproblem _1DISND_THM (in landau2)
 (conclusion conc _1DISND)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISNUM _N_ISTS1)

"))

(th~defproblem _NDIS1D_THM (in landau2)
 (conclusion conc _NDIS1D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_1DISND _SYMIS)

"))

(th~defproblem _N2ISND_THM (in landau2)
 (conclusion conc _N2ISND)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_ISDEN _N_ISTS2)

"))

(th~defproblem _NDISN2_THM (in landau2)
 (conclusion conc _NDISN2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_N2ISND _SYMIS)

"))

(th~defproblem _ISN_THM (in landau2)
 (conclusion conc _ISN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _ISD_THM (in landau2)
 (conclusion conc _ISD)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _ISND_THM (in landau2)
 (conclusion conc _ISND)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISD _ISN _TRIS)

"))

(th~defproblem _EQI12_THM (in landau2)
 (conclusion conc _EQI12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_12ISND _NDIS12 _TR3IS)

"))

(th~defproblem _EQI1_THM (in landau2)
 (conclusion conc _EQI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_FRIS _EQI12 _ISP)

"))

(th~defproblem _EQI2_THM (in landau2)
 (conclusion conc _EQI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_FRIS _EQI12 _ISP)

"))

(th~defproblem _SATZ37_THM (in landau2)
 (conclusion conc _SATZ37)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_REFIS)

"))

(th~defproblem _REFEQ_THM (in landau2)
 (conclusion conc _REFEQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ37)

"))

(th~defproblem _REFEQ1_THM (in landau2)
 (conclusion conc _REFEQ1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_REFEQ _ISP)

"))

(th~defproblem _REFEQ2_THM (in landau2)
 (conclusion conc _REFEQ2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_REFEQ _ISP)

"))

(th~defproblem _EQND_THM (in landau2)
 (conclusion conc _EQND)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISND _REFEQ1)

"))

(th~defproblem _EQN_THM (in landau2)
 (conclusion conc _EQN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISN _REFEQ1)

"))

(th~defproblem _EQD_THM (in landau2)
 (conclusion conc _EQD)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISD _REFEQ1)

"))

(th~defproblem _SATZ38_THM (in landau2)
 (conclusion conc _SATZ38)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SYMIS)

"))

(th~defproblem _SYMEQ_THM (in landau2)
 (conclusion conc _SYMEQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ38)

"))

(th~defproblem _II1_T1_THM (in landau2)
 (conclusion conc _II1_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _ASSTS2 _TRIS)

"))

(th~defproblem _STETS_THM (in landau2)
 (conclusion conc _STETS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _N_ISTS2 _ASSTS2 _II1_T1 _ASSTS1 _TR4IS)

"))

(th~defproblem _II1_T2_THM (in landau2)
 (conclusion conc _II1_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSTS1 _COMTS _N_ISTS1 _TR3IS)

"))

(th~defproblem _II1_ANDERS_THM (in landau2)
 (conclusion conc _II1_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSTS2 _II1_T2 _N_ISTS2 _ASSTS1 _TR3IS)

"))

(th~defproblem _139_T1_THM (in landau2)
 (conclusion conc _139_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISTS12)

"))

(th~defproblem _139_T2_THM (in landau2)
 (conclusion conc _139_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_STETS)

"))

(th~defproblem _139_T3_THM (in landau2)
 (conclusion conc _139_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _STETS _TRIS)

"))

(th~defproblem _139_T4_THM (in landau2)
 (conclusion conc _139_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_139_T3 _139_T1 _139_T2 _SYMIS _TR3IS)

"))

(th~defproblem _SATZ39_THM (in landau2)
 (conclusion conc _SATZ39)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_139_T4 _SATZ33B)

"))

(th~defproblem _139_ANDERS_THM (in landau2)
 (conclusion conc _139_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _STETS _ISTS12 _TR4IS)

"))

(th~defproblem _TREQ_THM (in landau2)
 (conclusion conc _TREQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ39)

"))

(th~defproblem _TREQ1_THM (in landau2)
 (conclusion conc _TREQ1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMEQ _TREQ)

"))

(th~defproblem _TREQ2_THM (in landau2)
 (conclusion conc _TREQ2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMEQ _TREQ)

"))

(th~defproblem _TR3EQ_THM (in landau2)
 (conclusion conc _TR3EQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TREQ)

"))

(th~defproblem _TR4EQ_THM (in landau2)
 (conclusion conc _TR4EQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TREQ _TR3EQ)

"))

(th~defproblem _SATZ40_THM (in landau2)
 (conclusion conc _SATZ40)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ASSTS2 _COMTS _N_ISTS2 _TRIS _EQI1)

"))

(th~defproblem _SATZ40A_THM (in landau2)
 (conclusion conc _SATZ40A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ40 _SYMEQ)

"))

(th~defproblem _SATZ40B_THM (in landau2)
 (conclusion conc _SATZ40B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_ASSTS2 _COMTS _N_ISTS2 _TRIS _EQI12)

"))

(th~defproblem _SATZ40C_THM (in landau2)
 (conclusion conc _SATZ40C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ40B _SYMEQ)

"))

(th~defproblem _MOREFI12_THM (in landau2)
 (conclusion conc _MOREFI12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_12ISND _ISMORE12)

"))

(th~defproblem _LESSFI12_THM (in landau2)
 (conclusion conc _LESSFI12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_12ISND _ISLESS12)

"))

(th~defproblem _MOREFI1_THM (in landau2)
 (conclusion conc _MOREFI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_1DISND _N2ISND _ISMORE12)

"))

(th~defproblem _MOREFI2_THM (in landau2)
 (conclusion conc _MOREFI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_N2ISND _1DISND _ISMORE12)

"))

(th~defproblem _LESSFI1_THM (in landau2)
 (conclusion conc _LESSFI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_1DISND _N2ISND _ISLESS12)

"))

(th~defproblem _LESSFI2_THM (in landau2)
 (conclusion conc _LESSFI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_N2ISND _1DISND _ISLESS12)

"))

(th~defproblem _SATZ41_THM (in landau2)
 (conclusion conc _SATZ41)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ10)

"))

(th~defproblem _SATZ41A_THM (in landau2)
 (conclusion conc _SATZ41A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ10A)

"))

(th~defproblem _SATZ41B_THM (in landau2)
 (conclusion conc _SATZ41B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ10B)

"))

(th~defproblem _SATZ42_THM (in landau2)
 (conclusion conc _SATZ42)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ11)

"))

(th~defproblem _SATZ43_THM (in landau2)
 (conclusion conc _SATZ43)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ12)

"))

(th~defproblem _244_T1_THM (in landau2)
 (conclusion conc _244_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SYMEQ _ISTS12)

"))

(th~defproblem _244_T2_THM (in landau2)
 (conclusion conc _244_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_STETS _244_T1 _TR3IS)

"))

(th~defproblem _244_T3_THM (in landau2)
 (conclusion conc _244_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32D _244_T2 _SYMIS _ISMORE1)

"))

(th~defproblem _SATZ44_THM (in landau2)
 (conclusion conc _SATZ44)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_244_T3 _COMTS _ISMORE1 _SATZ33A)

"))

(th~defproblem _EQMOREF12_THM (in landau2)
 (conclusion conc _EQMOREF12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ44)

"))

(th~defproblem _EQMOREF1_THM (in landau2)
 (conclusion conc _EQMOREF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ44)

"))

(th~defproblem _EQMOREF2_THM (in landau2)
 (conclusion conc _EQMOREF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ44)

"))

(th~defproblem _SATZ45_THM (in landau2)
 (conclusion conc _SATZ45)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ43 _SATZ44 _SATZ42)

"))

(th~defproblem _EQLESSF12_THM (in landau2)
 (conclusion conc _EQLESSF12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ45)

"))

(th~defproblem _EQLESSF1_THM (in landau2)
 (conclusion conc _EQLESSF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ45)

"))

(th~defproblem _EQLESSF2_THM (in landau2)
 (conclusion conc _EQLESSF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ45)

"))

(th~defproblem _MOREQI2_THM (in landau2)
 (conclusion conc _MOREQI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _LESSEQI2_THM (in landau2)
 (conclusion conc _LESSEQI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _MOREQI1_THM (in landau2)
 (conclusion conc _MOREQI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _LESSEQI1_THM (in landau2)
 (conclusion conc _LESSEQI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _SATZ41C_THM (in landau2)
 (conclusion conc _SATZ41C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMOR _SATZ41B _EC3_TH7)

"))

(th~defproblem _SATZ41D_THM (in landau2)
 (conclusion conc _SATZ41D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ41B _EC3_TH9)

"))

(th~defproblem _SATZ41E_THM (in landau2)
 (conclusion conc _SATZ41E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ41A _OR3_TH2)

"))

(th~defproblem _SATZ41F_THM (in landau2)
 (conclusion conc _SATZ41F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ41A _OR3_TH3 _COMOR)

"))

(th~defproblem _SATZ41G_THM (in landau2)
 (conclusion conc _SATZ41G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ41B _EC3E21 _EC3E23 _OR_TH3)

"))

(th~defproblem _SATZ41H_THM (in landau2)
 (conclusion conc _SATZ41H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ41B _EC3E31 _EC3E32 _OR_TH3)

"))

(th~defproblem _SATZ41J_THM (in landau2)
 (conclusion conc _SATZ41J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_OR_TH4 _OR_TH5 _SATZ41A _OR3E3)

"))

(th~defproblem _SATZ41K_THM (in landau2)
 (conclusion conc _SATZ41K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_OR_TH5 _OR_TH4 _SATZ41A _OR3E2)

"))

(th~defproblem _246_T1_THM (in landau2)
 (conclusion conc _246_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ44 _ORI1)

"))

(th~defproblem _246_T2_THM (in landau2)
 (conclusion conc _246_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMEQ _TR3EQ _ORI2)

"))

(th~defproblem _SATZ46_THM (in landau2)
 (conclusion conc _SATZ46)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_246_T2 _246_T1 _ORAPP)

"))

(th~defproblem _EQMOREQ12_THM (in landau2)
 (conclusion conc _EQMOREQ12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ46)

"))

(th~defproblem _EQMOREQ1_THM (in landau2)
 (conclusion conc _EQMOREQ1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ46)

"))

(th~defproblem _EQMOREQ2_THM (in landau2)
 (conclusion conc _EQMOREQ2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ46)

"))

(th~defproblem _247_T1_THM (in landau2)
 (conclusion conc _247_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ45 _ORI1)

"))

(th~defproblem _247_T2_THM (in landau2)
 (conclusion conc _247_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMEQ _TR3EQ _ORI2)

"))

(th~defproblem _SATZ47_THM (in landau2)
 (conclusion conc _SATZ47)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_247_T2 _247_T1 _ORAPP)

"))

(th~defproblem _EQLESSEQ12_THM (in landau2)
 (conclusion conc _EQLESSEQ12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ47)

"))

(th~defproblem _EQLESSEQ1_THM (in landau2)
 (conclusion conc _EQLESSEQ1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ47)

"))

(th~defproblem _EQLESSEQ2_THM (in landau2)
 (conclusion conc _EQLESSEQ2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _SATZ47)

"))

(th~defproblem _SATZ48_THM (in landau2)
 (conclusion conc _SATZ48)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ38 _SATZ42 _TH9)

"))

(th~defproblem _SATZ49_THM (in landau2)
 (conclusion conc _SATZ49)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ38 _SATZ43 _TH9)

"))

(th~defproblem _250_T1_THM (in landau2)
 (conclusion conc _250_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ34A)

"))

(th~defproblem _250_T2_THM (in landau2)
 (conclusion conc _250_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_250_T1 _COMTS _STETS _TRIS _ISLESS12)

"))

(th~defproblem _SATZ50_THM (in landau2)
 (conclusion conc _SATZ50)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_250_T2 _SATZ33C)

"))

(th~defproblem _TRLESSF_THM (in landau2)
 (conclusion conc _TRLESSF)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ50)

"))

(th~defproblem _TRMOREF_THM (in landau2)
 (conclusion conc _TRMOREF)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ42 _SATZ50 _SATZ43)

"))

(th~defproblem _SATZ51A_THM (in landau2)
 (conclusion conc _SATZ51A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMEQ _EQLESSF1 _SATZ50 _ORAPP)

"))

(th~defproblem _SATZ51B_THM (in landau2)
 (conclusion conc _SATZ51B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EQLESSF2 _SATZ50 _ORAPP)

"))

(th~defproblem _SATZ51C_THM (in landau2)
 (conclusion conc _SATZ51C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ48 _SATZ42 _SATZ51B _SATZ43)

"))

(th~defproblem _SATZ51D_THM (in landau2)
 (conclusion conc _SATZ51D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ42 _SATZ48 _SATZ51A _SATZ43)

"))

(th~defproblem _252_T1_THM (in landau2)
 (conclusion conc _252_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TREQ _ORI2)

"))

(th~defproblem _252_T2_THM (in landau2)
 (conclusion conc _252_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ51A _ORI1)

"))

(th~defproblem _252_T3_THM (in landau2)
 (conclusion conc _252_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_252_T1 _252_T2 _ORAPP)

"))

(th~defproblem _252_T4_THM (in landau2)
 (conclusion conc _252_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ51B _ORI1)

"))

(th~defproblem _SATZ52_THM (in landau2)
 (conclusion conc _SATZ52)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_252_T3 _252_T4 _ORAPP)

"))

(th~defproblem _TRLESSEQ_THM (in landau2)
 (conclusion conc _TRLESSEQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ52)

"))

(th~defproblem _252_T5_THM (in landau2)
 (conclusion conc _252_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ51B _ORI1)

"))

(th~defproblem _252_T6_THM (in landau2)
 (conclusion conc _252_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMEQ _EQLESSEQ1)

"))

(th~defproblem _252_ANDERS_THM (in landau2)
 (conclusion conc _252_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_252_T6 _252_T5 _ORAPP)

"))

(th~defproblem _TRMOREQ_THM (in landau2)
 (conclusion conc _TRMOREQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ48 _SATZ52 _SATZ49)

"))

(th~defproblem _253_T1_THM (in landau2)
 (conclusion conc _253_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ18 _DISTPT1 _ISMORE1)

"))

(th~defproblem _253_T2_THM (in landau2)
 (conclusion conc _253_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_253_T1 _MOREFI2)

"))

(th~defproblem _SATZ53_THM (in landau2)
 (conclusion conc _SATZ53)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_253_T2 _SOMEI)

"))

(th~defproblem _254_T1_THM (in landau2)
 (conclusion conc _254_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ18A _DISTPT2 _ISLESS2)

"))

(th~defproblem _254_T2_THM (in landau2)
 (conclusion conc _254_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_254_T1 _LESSFI2)

"))

(th~defproblem _SATZ54_THM (in landau2)
 (conclusion conc _SATZ54)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_254_T2 _SOMEI)

"))

(th~defproblem _255_T1_THM (in landau2)
 (conclusion conc _255_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19F)

"))

(th~defproblem _255_T2_THM (in landau2)
 (conclusion conc _255_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ19C)

"))

(th~defproblem _255_T3_THM (in landau2)
 (conclusion conc _255_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_255_T1 _DISTPT1 _DISTPT2 _ISLESS12)

"))

(th~defproblem _255_T4_THM (in landau2)
 (conclusion conc _255_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_255_T3 _LESSFI1)

"))

(th~defproblem _255_T5_THM (in landau2)
 (conclusion conc _255_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_255_T2 _DISTPT2 _DISTPT1 _ISLESS12)

"))

(th~defproblem _255_T6_THM (in landau2)
 (conclusion conc _255_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_255_T5 _LESSFI2)

"))

(th~defproblem _255_T7_THM (in landau2)
 (conclusion conc _255_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_255_T6 _255_T4 _ANDI)

"))

(th~defproblem _SATZ55_THM (in landau2)
 (conclusion conc _SATZ55)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_255_T7 _SOMEI)

"))

(th~defproblem _II3_T1_THM (in landau2)
 (conclusion conc _II3_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_NDIS12 _ISPL12)

"))

(th~defproblem _II3_T2_THM (in landau2)
 (conclusion conc _II3_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_DENIS _ISTS12)

"))

(th~defproblem _PF12_THM (in landau2)
 (conclusion conc _PF12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_II3_T2 _II3_T1 _ISND)

"))

(th~defproblem _II3_T3_THM (in landau2)
 (conclusion conc _II3_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_NDIS1D _NDISN2 _ISPL12)

"))

(th~defproblem _II3_T4_THM (in landau2)
 (conclusion conc _II3_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_DENIS _N_ISTS2)

"))

(th~defproblem _PF1_THM (in landau2)
 (conclusion conc _PF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_II3_T4 _II3_T3 _ISND)

"))

(th~defproblem _II3_T5_THM (in landau2)
 (conclusion conc _II3_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_NDISN2 _NDIS1D _ISPL12)

"))

(th~defproblem _II3_T6_THM (in landau2)
 (conclusion conc _II3_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_DENIS _N_ISTS1)

"))

(th~defproblem _PF2_THM (in landau2)
 (conclusion conc _PF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_II3_T6 _II3_T5 _ISND)

"))

(th~defproblem _PFEQ12A_THM (in landau2)
 (conclusion conc _PFEQ12A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PF12 _REFEQ1)

"))

(th~defproblem _PFEQ12B_THM (in landau2)
 (conclusion conc _PFEQ12B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PF12 _REFEQ2)

"))

(th~defproblem _PFEQ1A_THM (in landau2)
 (conclusion conc _PFEQ1A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PF1 _REFEQ1)

"))

(th~defproblem _PFEQ1B_THM (in landau2)
 (conclusion conc _PFEQ1B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PF1 _REFEQ2)

"))

(th~defproblem _PFEQ2A_THM (in landau2)
 (conclusion conc _PFEQ2A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PF2 _REFEQ1)

"))

(th~defproblem _PFEQ2B_THM (in landau2)
 (conclusion conc _PFEQ2B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_PF2 _REFEQ2)

"))

(th~defproblem _356_T1_THM (in landau2)
 (conclusion conc _356_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_N_ISTS1)

"))

(th~defproblem _356_T2_THM (in landau2)
 (conclusion conc _356_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_356_T1)

"))

(th~defproblem _356_T3_THM (in landau2)
 (conclusion conc _356_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _N_ISTS2 _STETS _TR3IS)

"))

(th~defproblem _356_T4_THM (in landau2)
 (conclusion conc _356_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _N_ISTS2 _STETS _356_T1 _356_T3 _TR4IS)

"))

(th~defproblem _356_T5_THM (in landau2)
 (conclusion conc _356_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_STETS _356_T2 _COMTS _N_ISTS2 _TR4IS)

"))

(th~defproblem _356_T6_THM (in landau2)
 (conclusion conc _356_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_356_T5 _356_T4 _ISPL12)

"))

(th~defproblem _356_T7_THM (in landau2)
 (conclusion conc _356_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_DISTPT1 _356_T6 _DISTTP1 _TR3IS)

"))

(th~defproblem _SATZ56_THM (in landau2)
 (conclusion conc _SATZ56)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_356_T7 _EQI12)

"))

(th~defproblem _EQPF12_THM (in landau2)
 (conclusion conc _EQPF12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ56)

"))

(th~defproblem _EQPF1_THM (in landau2)
 (conclusion conc _EQPF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_REFEQ _EQPF12)

"))

(th~defproblem _EQPF2_THM (in landau2)
 (conclusion conc _EQPF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_REFEQ _EQPF12)

"))

(th~defproblem _SATZ57_THM (in landau2)
 (conclusion conc _SATZ57)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ40C _DISTPT1 _EQN _PFEQ12A _TR3EQ)

"))

(th~defproblem _SATZ57A_THM (in landau2)
 (conclusion conc _SATZ57A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ57 _SYMEQ)

"))

(th~defproblem _SATZ58_THM (in landau2)
 (conclusion conc _SATZ58)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _COMPL _EQND)

"))

(th~defproblem _COMPF_THM (in landau2)
 (conclusion conc _COMPF)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ58)

"))

(th~defproblem _359_T1_THM (in landau2)
 (conclusion conc _359_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _ASSTS1 _N_ISTS1 _TR3IS)

"))

(th~defproblem _359_T2_THM (in landau2)
 (conclusion conc _359_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_359_T1 _ASSTS1 _ISPL12)

"))

(th~defproblem _359_T3_THM (in landau2)
 (conclusion conc _359_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_359_T2 _DISTTP1 _TRIS)

"))

(th~defproblem _359_T4_THM (in landau2)
 (conclusion conc _359_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSTS2 _COMTS _N_ISTS2 _TRIS)

"))

(th~defproblem _359_T5_THM (in landau2)
 (conclusion conc _359_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_359_T4 _359_T3 _ISPL12)

"))

(th~defproblem _359_T6_THM (in landau2)
 (conclusion conc _359_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_DISTPT1 _ISPL2)

"))

(th~defproblem _359_T7_THM (in landau2)
 (conclusion conc _359_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_359_T6 _ASSPL1 _359_T5 _TR3IS)

"))

(th~defproblem _SATZ59_THM (in landau2)
 (conclusion conc _SATZ59)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PFEQ1B _ASSTS1 _359_T7 _EQND _PFEQ2A _TR3EQ)

"))

(th~defproblem _ASSPF1_THM (in landau2)
 (conclusion conc _ASSPF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ59)

"))

(th~defproblem _ASSPF2_THM (in landau2)
 (conclusion conc _ASSPF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ASSPF1 _SYMEQ)

"))

(th~defproblem _STETS1_THM (in landau2)
 (conclusion conc _STETS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ASSTS2 _COMTS _N_ISTS2 _ASSTS1 _TR3IS)

"))

(th~defproblem _359_T8_THM (in landau2)
 (conclusion conc _359_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_STETS1 _ASSTS1 _ISPL12 _DISTTP1 _TRIS)

"))

(th~defproblem _359_T9_THM (in landau2)
 (conclusion conc _359_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_STETS1 _ASSTS2 _TRIS)

"))

(th~defproblem _ANDERST7_THM (in landau2)
 (conclusion conc _ANDERST7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_DISTPT1 _ISPL2 _ASSPL1 _359_T9 _359_T8 _ISPL12 _TR3IS)

"))

(th~defproblem _360_T1_THM (in landau2)
 (conclusion conc _360_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ18)

"))

(th~defproblem _360_T2_THM (in landau2)
 (conclusion conc _360_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_360_T1 _SATZ32A)

"))

(th~defproblem _360_T3_THM (in landau2)
 (conclusion conc _360_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _N_ISTS2 _ASSTS1 _TRIS)

"))

(th~defproblem _360_T4_THM (in landau2)
 (conclusion conc _360_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_360_T2 _360_T3 _ISMORE2)

"))

(th~defproblem _SATZ60_THM (in landau2)
 (conclusion conc _SATZ60)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_360_T4 _MOREFI2)

"))

(th~defproblem _SATZ60A_THM (in landau2)
 (conclusion conc _SATZ60A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ60 _SATZ42)

"))

(th~defproblem _361_T1_THM (in landau2)
 (conclusion conc _361_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32A)

"))

(th~defproblem _361_T2_THM (in landau2)
 (conclusion conc _361_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_361_T1 _STETS1 _ISMORE12)

"))

(th~defproblem _361_T3_THM (in landau2)
 (conclusion conc _361_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_STETS1)

"))

(th~defproblem _361_T4_THM (in landau2)
 (conclusion conc _361_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_361_T2 _361_T3 _SATZ19H)

"))

(th~defproblem _361_T5_THM (in landau2)
 (conclusion conc _361_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_361_T4 _DISTPT1 _ISMORE12)

"))

(th~defproblem _361_T6_THM (in landau2)
 (conclusion conc _361_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_361_T5 _SATZ32A)

"))

(th~defproblem _361_T7_THM (in landau2)
 (conclusion conc _361_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_361_T6 _ASSTS1 _ISMORE12)

"))

(th~defproblem _SATZ61_THM (in landau2)
 (conclusion conc _SATZ61)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_361_T7 _MOREFI12)

"))

(th~defproblem _SATZ62A_THM (in landau2)
 (conclusion conc _SATZ62A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ61)

"))

(th~defproblem _SATZ62B_THM (in landau2)
 (conclusion conc _SATZ62B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_EQPF1)

"))

(th~defproblem _SATZ62C_THM (in landau2)
 (conclusion conc _SATZ62C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ43 _SATZ61 _SATZ42)

"))

(th~defproblem _SATZ62D_THM (in landau2)
 (conclusion conc _SATZ62D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62A _COMPF _EQMOREF12)

"))

(th~defproblem _SATZ62E_THM (in landau2)
 (conclusion conc _SATZ62E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_EQPF2)

"))

(th~defproblem _SATZ62F_THM (in landau2)
 (conclusion conc _SATZ62F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62C _COMPF _EQLESSF12)

"))

(th~defproblem _SATZ62G_THM (in landau2)
 (conclusion conc _SATZ62G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62D _EQPF1 _EQMOREF2)

"))

(th~defproblem _SATZ62H_THM (in landau2)
 (conclusion conc _SATZ62H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62G _COMPF _EQMOREF12)

"))

(th~defproblem _SATZ62J_THM (in landau2)
 (conclusion conc _SATZ62J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62F _EQPF1 _EQLESSF2)

"))

(th~defproblem _SATZ62K_THM (in landau2)
 (conclusion conc _SATZ62K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62J _COMPF _EQLESSF12)

"))

(th~defproblem _363_T1_THM (in landau2)
 (conclusion conc _363_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ41A)

"))

(th~defproblem _363_T2_THM (in landau2)
 (conclusion conc _363_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ41B)

"))

(th~defproblem _SATZ63A_THM (in landau2)
 (conclusion conc _SATZ63A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62C _SATZ62A _SATZ62B _363_T2 _363_T1 _EC3_TH11)

"))

(th~defproblem _SATZ63B_THM (in landau2)
 (conclusion conc _SATZ63B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62C _SATZ62A _SATZ62B _363_T2 _363_T1 _EC3_TH10)

"))

(th~defproblem _SATZ63C_THM (in landau2)
 (conclusion conc _SATZ63C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62C _SATZ62A _SATZ62B _363_T2 _363_T1 _EC3_TH12)

"))

(th~defproblem _SATZ63D_THM (in landau2)
 (conclusion conc _SATZ63D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMPF _EQMOREF12 _SATZ63A)

"))

(th~defproblem _SATZ63E_THM (in landau2)
 (conclusion conc _SATZ63E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMPF _TR3EQ _SATZ63B)

"))

(th~defproblem _SATZ63F_THM (in landau2)
 (conclusion conc _SATZ63F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMPF _EQLESSF12 _SATZ63C)

"))

(th~defproblem _364_T1_THM (in landau2)
 (conclusion conc _364_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ61)

"))

(th~defproblem _364_T2_THM (in landau2)
 (conclusion conc _364_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ61 _COMPF _EQMOREF12)

"))

(th~defproblem _SATZ64_THM (in landau2)
 (conclusion conc _SATZ64)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_364_T2 _364_T1 _TRMOREF)

"))

(th~defproblem _SATZ64A_THM (in landau2)
 (conclusion conc _SATZ64A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ43 _SATZ64 _SATZ42)

"))

(th~defproblem _SATZ65A_THM (in landau2)
 (conclusion conc _SATZ65A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ62G _SATZ64 _ORAPP)

"))

(th~defproblem _SATZ65B_THM (in landau2)
 (conclusion conc _SATZ65B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ61 _EQPF2 _EQMOREF2 _SATZ64 _ORAPP)

"))

(th~defproblem _SATZ65C_THM (in landau2)
 (conclusion conc _SATZ65C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ43 _SATZ49 _SATZ65A _SATZ42)

"))

(th~defproblem _SATZ65D_THM (in landau2)
 (conclusion conc _SATZ65D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ49 _SATZ43 _SATZ65B _SATZ42)

"))

(th~defproblem _366_T1_THM (in landau2)
 (conclusion conc _366_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ56 _MOREQI2)

"))

(th~defproblem _366_T2_THM (in landau2)
 (conclusion conc _366_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ65A _MOREQI1)

"))

(th~defproblem _366_T3_THM (in landau2)
 (conclusion conc _366_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_366_T1 _366_T2 _ORAPP)

"))

(th~defproblem _366_T4_THM (in landau2)
 (conclusion conc _366_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ65B _MOREQI1)

"))

(th~defproblem _SATZ66_THM (in landau2)
 (conclusion conc _SATZ66)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_366_T3 _366_T4 _ORAPP)

"))

(th~defproblem _SATZ66A_THM (in landau2)
 (conclusion conc _SATZ66A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ49 _SATZ66 _SATZ48)

"))

(th~defproblem _367_T1_THM (in landau2)
 (conclusion conc _367_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ60 _EQMOREF1)

"))

(th~defproblem _367_T2_THM (in landau2)
 (conclusion conc _367_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_367_T1 _SATZ41D _TH3)

"))

(th~defproblem _VORBEMERKUNG67_THM (in landau2)
 (conclusion conc _VORBEMERKUNG67)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_367_T2 _SOME_TH5)

"))

(th~defproblem _SATZ67B_THM (in landau2)
 (conclusion conc _SATZ67B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TREQ2 _SATZ63E)

"))

(th~defproblem _367_T3_THM (in landau2)
 (conclusion conc _367_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_SATZ8B _ONEI)

"))

(th~defproblem _367_T4_THM (in landau2)
 (conclusion conc _367_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX4 _AX5)
Should be provable from hyps (_367_T3)

"))

(th~defproblem _367_T5_THM (in landau2)
 (conclusion conc _367_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMTS _EQD _SATZ40 _TREQ)

"))

(th~defproblem _367_T6_THM (in landau2)
 (conclusion conc _367_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ40A _367_T4 _SYMIS _EQN _SATZ57 _367_T5 _EQPF1 _TR4EQ)

"))

(th~defproblem _SATZ67A_THM (in landau2)
 (conclusion conc _SATZ67A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_367_T6 _SOMEI)

"))

(th~defproblem _SATZ67C_THM (in landau2)
 (conclusion conc _SATZ67C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_367_T6)

"))

(th~defproblem _SATZ67D_THM (in landau2)
 (conclusion conc _SATZ67D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ67C _SYMEQ)

"))

(th~defproblem _SATZ67E_THM (in landau2)
 (conclusion conc _SATZ67E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ67C _SATZ67B)

"))

(th~defproblem _II4_T1_THM (in landau2)
 (conclusion conc _II4_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_NUMIS _ISTS12)

"))

(th~defproblem _II4_T2_THM (in landau2)
 (conclusion conc _II4_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_DENIS _ISTS12)

"))

(th~defproblem _TF12_THM (in landau2)
 (conclusion conc _TF12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_II4_T2 _II4_T1 _ISND)

"))

(th~defproblem _II4_T3_THM (in landau2)
 (conclusion conc _II4_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_NUMIS _N_ISTS2)

"))

(th~defproblem _II4_T4_THM (in landau2)
 (conclusion conc _II4_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_DENIS _N_ISTS2)

"))

(th~defproblem _TF1_THM (in landau2)
 (conclusion conc _TF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_II4_T4 _II4_T3 _ISND)

"))

(th~defproblem _II4_T5_THM (in landau2)
 (conclusion conc _II4_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_NUMIS _N_ISTS1)

"))

(th~defproblem _II4_T6_THM (in landau2)
 (conclusion conc _II4_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_DENIS _N_ISTS1)

"))

(th~defproblem _TF2_THM (in landau2)
 (conclusion conc _TF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_II4_T6 _II4_T5 _ISND)

"))

(th~defproblem _TFEQ12A_THM (in landau2)
 (conclusion conc _TFEQ12A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_TF12 _REFEQ1)

"))

(th~defproblem _TFEQ12B_THM (in landau2)
 (conclusion conc _TFEQ12B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_TF12 _REFEQ2)

"))

(th~defproblem _TFEQ1A_THM (in landau2)
 (conclusion conc _TFEQ1A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_TF1 _REFEQ1)

"))

(th~defproblem _TFEQ1B_THM (in landau2)
 (conclusion conc _TFEQ1B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_TF1 _REFEQ2)

"))

(th~defproblem _TFEQ2A_THM (in landau2)
 (conclusion conc _TFEQ2A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_TF2 _REFEQ1)

"))

(th~defproblem _TFEQ2B_THM (in landau2)
 (conclusion conc _TFEQ2B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_TF2 _REFEQ2)

"))

(th~defproblem _468_T1_THM (in landau2)
 (conclusion conc _468_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_ISTS12)

"))

(th~defproblem _STETS2_THM (in landau2)
 (conclusion conc _STETS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _N_ISTS2 _STETS _TR3IS)

"))

(th~defproblem _468_T2_THM (in landau2)
 (conclusion conc _468_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_STETS2 _468_T1 _TR3IS)

"))

(th~defproblem _SATZ68_THM (in landau2)
 (conclusion conc _SATZ68)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_468_T2 _EQI12)

"))

(th~defproblem _EQTF12_THM (in landau2)
 (conclusion conc _EQTF12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ68)

"))

(th~defproblem _EQTF1_THM (in landau2)
 (conclusion conc _EQTF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_REFEQ _EQTF12)

"))

(th~defproblem _EQTF2_THM (in landau2)
 (conclusion conc _EQTF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_REFEQ _EQTF12)

"))

(th~defproblem _SATZ69_THM (in landau2)
 (conclusion conc _SATZ69)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_COMTS _EQND)

"))

(th~defproblem _COMTF_THM (in landau2)
 (conclusion conc _COMTF)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ69)

"))

(th~defproblem _SATZ70_THM (in landau2)
 (conclusion conc _SATZ70)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TFEQ1B _ASSTS1 _EQND _TFEQ2A _TR3EQ)

"))

(th~defproblem _ASSTF1_THM (in landau2)
 (conclusion conc _ASSTF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ70)

"))

(th~defproblem _ASSTF2_THM (in landau2)
 (conclusion conc _ASSTF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ASSTF1 _SYMEQ)

"))

(th~defproblem _471_T1_THM (in landau2)
 (conclusion conc _471_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ57A _DISTTP2 _EQN _TFEQ1A _TR3EQ)

"))

(th~defproblem _471_T2_THM (in landau2)
 (conclusion conc _471_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ40C _ASSTS2 _EQND _TREQ)

"))

(th~defproblem _471_T3_THM (in landau2)
 (conclusion conc _471_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_471_T2 _COMTS _N_ISTS2 _EQD _TREQ)

"))

(th~defproblem _SATZ71_THM (in landau2)
 (conclusion conc _SATZ71)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_471_T3 _471_T2 _EQPF12 _471_T1 _TREQ)

"))

(th~defproblem _DISTTPF1_THM (in landau2)
 (conclusion conc _DISTTPF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMTF _EQPF12 _SATZ71 _TR3EQ)

"))

(th~defproblem _DISTTPF2_THM (in landau2)
 (conclusion conc _DISTTPF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ71)

"))

(th~defproblem _DISTPTF1_THM (in landau2)
 (conclusion conc _DISTPTF1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_DISTTPF1 _SYMEQ)

"))

(th~defproblem _DISTPTF2_THM (in landau2)
 (conclusion conc _DISTPTF2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_DISTTPF2 _SYMEQ)

"))

(th~defproblem _472_T1_THM (in landau2)
 (conclusion conc _472_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ32A)

"))

(th~defproblem _472_T2_THM (in landau2)
 (conclusion conc _472_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_472_T1 _STETS2 _ISMORE12)

"))

(th~defproblem _SATZ72A_THM (in landau2)
 (conclusion conc _SATZ72A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_472_T2 _MOREFI12)

"))

(th~defproblem _SATZ72B_THM (in landau2)
 (conclusion conc _SATZ72B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_REFEQ _SATZ68)

"))

(th~defproblem _SATZ72C_THM (in landau2)
 (conclusion conc _SATZ72C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ43 _SATZ72A _SATZ42)

"))

(th~defproblem _SATZ72D_THM (in landau2)
 (conclusion conc _SATZ72D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72A _COMTF _EQMOREF12)

"))

(th~defproblem _SATZ72E_THM (in landau2)
 (conclusion conc _SATZ72E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_EQTF2)

"))

(th~defproblem _SATZ72F_THM (in landau2)
 (conclusion conc _SATZ72F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72C _COMTF _EQLESSF12)

"))

(th~defproblem _SATZ72G_THM (in landau2)
 (conclusion conc _SATZ72G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72D _EQTF1 _EQMOREF2)

"))

(th~defproblem _SATZ72H_THM (in landau2)
 (conclusion conc _SATZ72H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72G _COMTF _EQMOREF12)

"))

(th~defproblem _SATZ72J_THM (in landau2)
 (conclusion conc _SATZ72J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72F _EQTF1 _EQLESSF2)

"))

(th~defproblem _SATZ72K_THM (in landau2)
 (conclusion conc _SATZ72K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72J _COMTF _EQLESSF12)

"))

(th~defproblem _473_T1_THM (in landau2)
 (conclusion conc _473_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SATZ41A)

"))

(th~defproblem _473_T2_THM (in landau2)
 (conclusion conc _473_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ41B)

"))

(th~defproblem _SATZ73A_THM (in landau2)
 (conclusion conc _SATZ73A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72C _SATZ72A _SATZ72B _473_T2 _473_T1 _EC3_TH11)

"))

(th~defproblem _SATZ73B_THM (in landau2)
 (conclusion conc _SATZ73B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72C _SATZ72A _SATZ72B _473_T2 _473_T1 _EC3_TH10)

"))

(th~defproblem _SATZ73C_THM (in landau2)
 (conclusion conc _SATZ73C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72C _SATZ72A _SATZ72B _473_T2 _473_T1 _EC3_TH12)

"))

(th~defproblem _SATZ73D_THM (in landau2)
 (conclusion conc _SATZ73D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMTF _EQMOREF12 _SATZ73A)

"))

(th~defproblem _SATZ73E_THM (in landau2)
 (conclusion conc _SATZ73E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMTF _TR3EQ _SATZ73B)

"))

(th~defproblem _SATZ73F_THM (in landau2)
 (conclusion conc _SATZ73F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMTF _EQLESSF12 _SATZ73C)

"))

(th~defproblem _474_T1_THM (in landau2)
 (conclusion conc _474_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ72A)

"))

(th~defproblem _474_T2_THM (in landau2)
 (conclusion conc _474_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72A _COMTF _EQMOREF12)

"))

(th~defproblem _SATZ74_THM (in landau2)
 (conclusion conc _SATZ74)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_474_T2 _474_T1 _TRMOREF)

"))

(th~defproblem _SATZ74A_THM (in landau2)
 (conclusion conc _SATZ74A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ43 _SATZ74 _SATZ42)

"))

(th~defproblem _SATZ75A_THM (in landau2)
 (conclusion conc _SATZ75A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72G _SATZ74 _ORAPP)

"))

(th~defproblem _SATZ75B_THM (in landau2)
 (conclusion conc _SATZ75B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ72A _EQTF2 _EQMOREF2 _SATZ74 _ORAPP)

"))

(th~defproblem _SATZ75C_THM (in landau2)
 (conclusion conc _SATZ75C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ43 _SATZ49 _SATZ75A _SATZ42)

"))

(th~defproblem _SATZ75D_THM (in landau2)
 (conclusion conc _SATZ75D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ49 _SATZ43 _SATZ75B _SATZ42)

"))

(th~defproblem _476_T1_THM (in landau2)
 (conclusion conc _476_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ68 _MOREQI2)

"))

(th~defproblem _476_T2_THM (in landau2)
 (conclusion conc _476_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ75A _MOREQI1)

"))

(th~defproblem _476_T3_THM (in landau2)
 (conclusion conc _476_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_476_T1 _476_T2 _ORAPP)

"))

(th~defproblem _476_T4_THM (in landau2)
 (conclusion conc _476_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ75B _MOREQI1)

"))

(th~defproblem _SATZ76_THM (in landau2)
 (conclusion conc _SATZ76)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_476_T3 _476_T4 _ORAPP)

"))

(th~defproblem _SATZ76A_THM (in landau2)
 (conclusion conc _SATZ76A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ49 _SATZ76 _SATZ48)

"))

(th~defproblem _SATZ77B_THM (in landau2)
 (conclusion conc _SATZ77B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TREQ2 _SATZ73E)

"))

(th~defproblem _477_T1_THM (in landau2)
 (conclusion conc _477_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ40A _ASSTS1 _COMTS _N_ISTS2 _TRIS _EQND _TFEQ2A _COMTF _TR4EQ)

"))

(th~defproblem _SATZ77A_THM (in landau2)
 (conclusion conc _SATZ77A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_477_T1 _SOMEI)

"))

(th~defproblem _RT_REFEQ_THM (in landau2)
 (conclusion conc _RT_REFEQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_REFEQ)

"))

(th~defproblem _RT_SYMEQ_THM (in landau2)
 (conclusion conc _RT_SYMEQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX5)
Should be provable from hyps (_SYMEQ)

"))

(th~defproblem _RT_TREQ_THM (in landau2)
 (conclusion conc _RT_TREQ)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TREQ)

"))

(th~defproblem _INCLASS_THM (in landau2)
 (conclusion conc _INCLASS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_TREQ _RT_SYMEQ _RT_REFEQ _4_TH5)

"))

(th~defproblem _LEMMAEQ1_THM (in landau2)
 (conclusion conc _LEMMAEQ1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_TREQ _RT_SYMEQ _RT_REFEQ _4_TH8)

"))

(th~defproblem _RATAPP1_THM (in landau2)
 (conclusion conc _RATAPP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_TREQ _RT_SYMEQ _RT_REFEQ _4_TH3)

"))

(th~defproblem _II5_T1_THM (in landau2)
 (conclusion conc _II5_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RATAPP1)

"))

(th~defproblem _RATAPP2_THM (in landau2)
 (conclusion conc _RATAPP2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T1 _RATAPP1)

"))

(th~defproblem _II5_T2_THM (in landau2)
 (conclusion conc _II5_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RATAPP2)

"))

(th~defproblem _RATAPP3_THM (in landau2)
 (conclusion conc _RATAPP3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T2 _RATAPP1)

"))

(th~defproblem _II5_T3_THM (in landau2)
 (conclusion conc _II5_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RATAPP3)

"))

(th~defproblem _RATAPP4_THM (in landau2)
 (conclusion conc _RATAPP4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T3 _RATAPP1)

"))

(th~defproblem _ISI_THM (in landau2)
 (conclusion conc _ISI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_TREQ _RT_SYMEQ _RT_REFEQ _5_TH3)

"))

(th~defproblem _ISE_THM (in landau2)
 (conclusion conc _ISE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_TREQ _RT_SYMEQ _RT_REFEQ _5_TH5)

"))

(th~defproblem _NISI_THM (in landau2)
 (conclusion conc _NISI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISE _TH3)

"))

(th~defproblem _NISE_THM (in landau2)
 (conclusion conc _NISE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISI _TH3)

"))

(th~defproblem _ISINDRAT_THM (in landau2)
 (conclusion conc (ALL-TYPES Z (FORALL (LAM (DUMMY Z) (_ISINDRAT DUMMY)))))
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_TREQ _RT_SYMEQ _RT_REFEQ _11_TH1)

"))

(th~defproblem _SATZ78_THM (in landau2)
 (conclusion conc _SATZ78)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS)

"))

(th~defproblem _SATZ79_THM (in landau2)
 (conclusion conc _SATZ79)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SYMIS)

"))

(th~defproblem _SATZ80_THM (in landau2)
 (conclusion conc _SATZ80)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TRIS)

"))

(th~defproblem _II5_T4_THM (in landau2)
 (conclusion conc _II5_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E1)

"))

(th~defproblem _II5_T5_THM (in landau2)
 (conclusion conc _II5_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E2)

"))

(th~defproblem _II5_T6_THM (in landau2)
 (conclusion conc _II5_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E3)

"))

(th~defproblem _II5_T7_THM (in landau2)
 (conclusion conc _II5_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _II5_T5 _ISE _II5_T4 _II5_T6 _SATZ44)

"))

(th~defproblem _II5_T8_THM (in landau2)
 (conclusion conc _II5_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T7 _SOMEAPP)

"))

(th~defproblem _ALSO18_THM (in landau2)
 (conclusion conc _ALSO18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T8 _SOMEAPP)

"))

(th~defproblem _II5_T9_THM (in landau2)
 (conclusion conc _II5_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3I)

"))

(th~defproblem _II5_T10_THM (in landau2)
 (conclusion conc _II5_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T9 _SOMEI)

"))

(th~defproblem _MOREI_THM (in landau2)
 (conclusion conc _MOREI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T10 _SOMEI)

"))

(th~defproblem _MOREE_THM (in landau2)
 (conclusion conc _MOREE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ALSO18)

"))

(th~defproblem _RT_ISMORE1_THM (in landau2)
 (conclusion conc _RT_ISMORE1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISMORE2_THM (in landau2)
 (conclusion conc _RT_ISMORE2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISMORE12_THM (in landau2)
 (conclusion conc _RT_ISMORE12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISMORE1 _RT_ISMORE2)

"))

(th~defproblem _II5_T11_THM (in landau2)
 (conclusion conc _II5_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E1)

"))

(th~defproblem _II5_T12_THM (in landau2)
 (conclusion conc _II5_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E2)

"))

(th~defproblem _II5_T13_THM (in landau2)
 (conclusion conc _II5_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3E3)

"))

(th~defproblem _II5_T14_THM (in landau2)
 (conclusion conc _II5_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _II5_T12 _ISE _II5_T11 _II5_T13 _SATZ45)

"))

(th~defproblem _II5_T15_THM (in landau2)
 (conclusion conc _II5_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T14 _SOMEAPP)

"))

(th~defproblem _ALSO19_THM (in landau2)
 (conclusion conc _ALSO19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T15 _SOMEAPP)

"))

(th~defproblem _II5_T16_THM (in landau2)
 (conclusion conc _II5_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_AND3I)

"))

(th~defproblem _II5_T17_THM (in landau2)
 (conclusion conc _II5_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T16 _SOMEI)

"))

(th~defproblem _LESSI_THM (in landau2)
 (conclusion conc _LESSI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T17 _SOMEI)

"))

(th~defproblem _LESSE_THM (in landau2)
 (conclusion conc _LESSE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ALSO19)

"))

(th~defproblem _RT_ISLESS1_THM (in landau2)
 (conclusion conc _RT_ISLESS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISLESS2_THM (in landau2)
 (conclusion conc _RT_ISLESS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISLESS12_THM (in landau2)
 (conclusion conc _RT_ISLESS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISLESS1 _RT_ISLESS2)

"))

(th~defproblem _581_T1_THM (in landau2)
 (conclusion conc _581_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ41A)

"))

(th~defproblem _581_T2_THM (in landau2)
 (conclusion conc _581_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISI _OR3I1)

"))

(th~defproblem _581_T3_THM (in landau2)
 (conclusion conc _581_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREI _OR3I2)

"))

(th~defproblem _581_T4_THM (in landau2)
 (conclusion conc _581_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSI _OR3I3)

"))

(th~defproblem _581_T5_THM (in landau2)
 (conclusion conc _581_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_581_T4 _581_T3 _581_T2 _581_T1 _OR3APP)

"))

(th~defproblem _581_T6_THM (in landau2)
 (conclusion conc _581_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ41B)

"))

(th~defproblem _581_T7_THM (in landau2)
 (conclusion conc _581_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _ISE _581_T6 _EC3E12 _TH3)

"))

(th~defproblem _581_T8_THM (in landau2)
 (conclusion conc _581_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSE _MOREE _581_T6 _EC3E23 _TH3)

"))

(th~defproblem _581_T9_THM (in landau2)
 (conclusion conc _581_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISE _LESSE _581_T6 _EC3E31 _TH3)

"))

(th~defproblem _581_T10_THM (in landau2)
 (conclusion conc _581_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_581_T9 _EC_TH1 _581_T8 _581_T7 _EC3_TH6)

"))

(th~defproblem _581_T11_THM (in landau2)
 (conclusion conc _581_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_581_T10 _581_T5 _OREC3I)

"))

(th~defproblem _SATZ81_THM (in landau2)
 (conclusion conc _SATZ81)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_581_T11 _RATAPP2)

"))

(th~defproblem _SATZ81A_THM (in landau2)
 (conclusion conc _SATZ81A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81 _OREC3E1)

"))

(th~defproblem _SATZ81B_THM (in landau2)
 (conclusion conc _SATZ81B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81 _OREC3E2)

"))

(th~defproblem _582_T1_THM (in landau2)
 (conclusion conc _582_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _SATZ42 _LESSI)

"))

(th~defproblem _SATZ82_THM (in landau2)
 (conclusion conc _SATZ82)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_582_T1 _RATAPP2)

"))

(th~defproblem _583_T1_THM (in landau2)
 (conclusion conc _583_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSE _SATZ43 _MOREI)

"))

(th~defproblem _SATZ83_THM (in landau2)
 (conclusion conc _SATZ83)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_583_T1 _RATAPP2)

"))

(th~defproblem _RT_MOREISI1_THM (in landau2)
 (conclusion conc _RT_MOREISI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _RT_MOREISI2_THM (in landau2)
 (conclusion conc _RT_MOREISI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _MOREISI_THM (in landau2)
 (conclusion conc _MOREISI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISI _RT_MOREISI2 _MOREI _RT_MOREISI1 _ORAPP)

"))

(th~defproblem _MOREISE_THM (in landau2)
 (conclusion conc _MOREISE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISE _MOREQI2 _MOREE _MOREQI1 _ORAPP)

"))

(th~defproblem _RT_ISMOREIS1_THM (in landau2)
 (conclusion conc _RT_ISMOREIS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISMOREIS2_THM (in landau2)
 (conclusion conc _RT_ISMOREIS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISMOREIS12_THM (in landau2)
 (conclusion conc _RT_ISMOREIS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISMOREIS1 _RT_ISMOREIS2)

"))

(th~defproblem _RT_LESSISI1_THM (in landau2)
 (conclusion conc _RT_LESSISI1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI1)

"))

(th~defproblem _RT_LESSISI2_THM (in landau2)
 (conclusion conc _RT_LESSISI2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ORI2)

"))

(th~defproblem _LESSISI_THM (in landau2)
 (conclusion conc _LESSISI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISI _RT_LESSISI2 _LESSI _RT_LESSISI1 _ORAPP)

"))

(th~defproblem _LESSISE_THM (in landau2)
 (conclusion conc _LESSISE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISE _LESSEQI2 _LESSE _LESSEQI1 _ORAPP)

"))

(th~defproblem _RT_ISLESSIS1_THM (in landau2)
 (conclusion conc _RT_ISLESSIS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISLESSIS2_THM (in landau2)
 (conclusion conc _RT_ISLESSIS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISP)

"))

(th~defproblem _RT_ISLESSIS12_THM (in landau2)
 (conclusion conc _RT_ISLESSIS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISLESSIS1 _RT_ISLESSIS2)

"))

(th~defproblem _SATZ81C_THM (in landau2)
 (conclusion conc _SATZ81C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_COMOR _SATZ81B _EC3_TH7)

"))

(th~defproblem _SATZ81D_THM (in landau2)
 (conclusion conc _SATZ81D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81B _EC3_TH9)

"))

(th~defproblem _SATZ81E_THM (in landau2)
 (conclusion conc _SATZ81E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81A _OR3_TH2)

"))

(th~defproblem _SATZ81F_THM (in landau2)
 (conclusion conc _SATZ81F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81A _OR3_TH3 _COMOR)

"))

(th~defproblem _SATZ81G_THM (in landau2)
 (conclusion conc _SATZ81G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81B _EC3E21 _EC3E23 _OR_TH3)

"))

(th~defproblem _SATZ81H_THM (in landau2)
 (conclusion conc _SATZ81H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ81B _EC3E31 _EC3E32 _OR_TH3)

"))

(th~defproblem _SATZ81J_THM (in landau2)
 (conclusion conc _SATZ81J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_OR_TH4 _OR_TH5 _SATZ81A _OR3E3)

"))

(th~defproblem _SATZ81K_THM (in landau2)
 (conclusion conc _SATZ81K)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_OR_TH5 _OR_TH4 _SATZ81A _OR3E2)

"))

(th~defproblem _584_T1_THM (in landau2)
 (conclusion conc _584_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREISE _SATZ48 _LESSISI)

"))

(th~defproblem _SATZ84_THM (in landau2)
 (conclusion conc _SATZ84)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_584_T1 _RATAPP2)

"))

(th~defproblem _585_T1_THM (in landau2)
 (conclusion conc _585_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSISE _SATZ49 _MOREISI)

"))

(th~defproblem _SATZ85_THM (in landau2)
 (conclusion conc _SATZ85)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_585_T1 _RATAPP2)

"))

(th~defproblem _586_T1_THM (in landau2)
 (conclusion conc _586_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSE _SATZ50 _LESSI)

"))

(th~defproblem _SATZ86_THM (in landau2)
 (conclusion conc _SATZ86)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_586_T1 _RATAPP3)

"))

(th~defproblem _RT_TRLESS_THM (in landau2)
 (conclusion conc _RT_TRLESS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ86)

"))

(th~defproblem _RT_TRMORE_THM (in landau2)
 (conclusion conc _RT_TRMORE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ82 _SATZ86 _SATZ83)

"))

(th~defproblem _587_T1_THM (in landau2)
 (conclusion conc _587_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSE _LESSISE _SATZ51A _LESSI)

"))

(th~defproblem _SATZ87A_THM (in landau2)
 (conclusion conc _SATZ87A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_587_T1 _RATAPP3)

"))

(th~defproblem _587_T2_THM (in landau2)
 (conclusion conc _587_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSISE _LESSE _SATZ51B _LESSI)

"))

(th~defproblem _SATZ87B_THM (in landau2)
 (conclusion conc _SATZ87B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_587_T2 _RATAPP3)

"))

(th~defproblem _SATZ87C_THM (in landau2)
 (conclusion conc _SATZ87C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ84 _SATZ82 _SATZ87B _SATZ83)

"))

(th~defproblem _SATZ87D_THM (in landau2)
 (conclusion conc _SATZ87D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ82 _SATZ84 _SATZ87A _SATZ83)

"))

(th~defproblem _588_T1_THM (in landau2)
 (conclusion conc _588_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSISE _SATZ52 _LESSISI)

"))

(th~defproblem _SATZ88_THM (in landau2)
 (conclusion conc _SATZ88)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_588_T1 _RATAPP3)

"))

(th~defproblem _RT_TRLESSIS_THM (in landau2)
 (conclusion conc _RT_TRLESSIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ88)

"))

(th~defproblem _RT_TRMOREIS_THM (in landau2)
 (conclusion conc _RT_TRMOREIS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ84 _SATZ88 _SATZ85)

"))

(th~defproblem _589_T1_THM (in landau2)
 (conclusion conc _589_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASS _MOREI _SOMEI)

"))

(th~defproblem _589_T2_THM (in landau2)
 (conclusion conc _589_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_589_T1 _SATZ53 _SOMEAPP)

"))

(th~defproblem _SATZ89_THM (in landau2)
 (conclusion conc _SATZ89)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_589_T2 _RATAPP1)

"))

(th~defproblem _590_T1_THM (in landau2)
 (conclusion conc _590_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASS _LESSI _SOMEI)

"))

(th~defproblem _590_T2_THM (in landau2)
 (conclusion conc _590_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_590_T1 _SATZ54 _SOMEAPP)

"))

(th~defproblem _SATZ90_THM (in landau2)
 (conclusion conc _SATZ90)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_590_T2 _RATAPP1)

"))

(th~defproblem _591_T1_THM (in landau2)
 (conclusion conc _591_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1 _INCLASS _LESSI)

"))

(th~defproblem _591_T2_THM (in landau2)
 (conclusion conc _591_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2 _INCLASS _LESSI)

"))

(th~defproblem _591_T3_THM (in landau2)
 (conclusion conc _591_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_591_T2 _591_T1 _ANDI)

"))

(th~defproblem _591_T4_THM (in landau2)
 (conclusion conc _591_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_591_T3 _SOMEI)

"))

(th~defproblem _591_T5_THM (in landau2)
 (conclusion conc _591_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_591_T4 _LESSE _SATZ55 _SOMEAPP)

"))

(th~defproblem _SATZ91_THM (in landau2)
 (conclusion conc _SATZ91)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_591_T5 _RATAPP2)

"))

(th~defproblem _II5_T18_THM (in landau2)
 (conclusion conc _II5_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ56 _INCLASS _ISI)

"))

(th~defproblem _FPLUSFRT_THM (in landau2)
 (conclusion conc _FPLUSFRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T18)

"))

(th~defproblem _II5_T19_THM (in landau2)
 (conclusion conc _II5_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_FPLUSFRT _ISINDRAT)

"))

(th~defproblem _PICP_THM (in landau2)
 (conclusion conc _PICP)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T19 _INCLASS _ISP)

"))

(th~defproblem _RT_ISPL1_THM (in landau2)
 (conclusion conc _RT_ISPL1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RT_ISPL2_THM (in landau2)
 (conclusion conc _RT_ISPL2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RT_ISPL12_THM (in landau2)
 (conclusion conc _RT_ISPL12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISPL2 _RT_ISPL1 _TRIS)

"))

(th~defproblem _592_T1_THM (in landau2)
 (conclusion conc _592_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ58 _PICP _ISI)

"))

(th~defproblem _SATZ92_THM (in landau2)
 (conclusion conc _SATZ92)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_592_T1 _RATAPP2)

"))

(th~defproblem _RT_COMPL_THM (in landau2)
 (conclusion conc _RT_COMPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ92)

"))

(th~defproblem _593_T1_THM (in landau2)
 (conclusion conc _593_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PICP)

"))

(th~defproblem _593_T2_THM (in landau2)
 (conclusion conc _593_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PICP)

"))

(th~defproblem _593_T3_THM (in landau2)
 (conclusion conc _593_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ59 _593_T2 _593_T1 _ISI)

"))

(th~defproblem _SATZ93_THM (in landau2)
 (conclusion conc _SATZ93)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_593_T3 _RATAPP3)

"))

(th~defproblem _RT_ASSPL1_THM (in landau2)
 (conclusion conc _RT_ASSPL1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ93)

"))

(th~defproblem _RT_ASSPL2_THM (in landau2)
 (conclusion conc _RT_ASSPL2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ93 _SYMIS)

"))

(th~defproblem _594_T1_THM (in landau2)
 (conclusion conc _594_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ60 _PICP _MOREI)

"))

(th~defproblem _SATZ94_THM (in landau2)
 (conclusion conc _SATZ94)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_594_T1 _RATAPP2)

"))

(th~defproblem _SATZ94A_THM (in landau2)
 (conclusion conc _SATZ94A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ94 _SATZ82)

"))

(th~defproblem _595_T1_THM (in landau2)
 (conclusion conc _595_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _SATZ61 _PICP _MOREI)

"))

(th~defproblem _SATZ95_THM (in landau2)
 (conclusion conc _SATZ95)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_595_T1 _RATAPP3)

"))

(th~defproblem _596_T1_THM (in landau2)
 (conclusion conc _596_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _SATZ62A _PICP _MOREI)

"))

(th~defproblem _SATZ96A_THM (in landau2)
 (conclusion conc _SATZ96A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_596_T1 _RATAPP3)

"))

(th~defproblem _596_T2_THM (in landau2)
 (conclusion conc _596_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISE _SATZ62B _PICP _ISI)

"))

(th~defproblem _SATZ96B_THM (in landau2)
 (conclusion conc _SATZ96B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_596_T2 _RATAPP3)

"))

(th~defproblem _596_T3_THM (in landau2)
 (conclusion conc _596_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSE _SATZ62C _PICP _LESSI)

"))

(th~defproblem _SATZ96C_THM (in landau2)
 (conclusion conc _SATZ96C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_596_T3 _RATAPP3)

"))

(th~defproblem _596_ANDERSA_THM (in landau2)
 (conclusion conc _596_ANDERSA)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ95)

"))

(th~defproblem _596_ANDERSB_THM (in landau2)
 (conclusion conc _596_ANDERSB)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISPL1)

"))

(th~defproblem _596_ANDERSC_THM (in landau2)
 (conclusion conc _596_ANDERSC)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ95 _SATZ82)

"))

(th~defproblem _SATZ96D_THM (in landau2)
 (conclusion conc _SATZ96D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ96A _RT_COMPL _RT_ISMORE12)

"))

(th~defproblem _SATZ96E_THM (in landau2)
 (conclusion conc _SATZ96E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISPL2)

"))

(th~defproblem _SATZ96F_THM (in landau2)
 (conclusion conc _SATZ96F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ96C _RT_COMPL _RT_ISLESS12)

"))

(th~defproblem _597_T1_THM (in landau2)
 (conclusion conc _597_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PICP _MOREE _SATZ63A _MOREI)

"))

(th~defproblem _SATZ97A_THM (in landau2)
 (conclusion conc _SATZ97A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_597_T1 _RATAPP3)

"))

(th~defproblem _597_T2_THM (in landau2)
 (conclusion conc _597_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PICP _ISE _SATZ63B _ISI)

"))

(th~defproblem _SATZ97B_THM (in landau2)
 (conclusion conc _SATZ97B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_597_T2 _RATAPP3)

"))

(th~defproblem _597_T3_THM (in landau2)
 (conclusion conc _597_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PICP _LESSE _SATZ63C _LESSI)

"))

(th~defproblem _SATZ97C_THM (in landau2)
 (conclusion conc _SATZ97C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_597_T3 _RATAPP3)

"))

(th~defproblem _597_ANDERS_THM (in landau2)
 (conclusion conc _597_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ97A _SATZ82)

"))

(th~defproblem _598_T1_THM (in landau2)
 (conclusion conc _598_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _SATZ64 _PICP _MOREI)

"))

(th~defproblem _SATZ98_THM (in landau2)
 (conclusion conc _SATZ98)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_598_T1 _RATAPP4)

"))

(th~defproblem _SATZ98A_THM (in landau2)
 (conclusion conc _SATZ98A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ98 _SATZ82)

"))

(th~defproblem _599_T1_THM (in landau2)
 (conclusion conc _599_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _MOREISE _SATZ65A _PICP _MOREI)

"))

(th~defproblem _SATZ99A_THM (in landau2)
 (conclusion conc _SATZ99A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_599_T1 _RATAPP4)

"))

(th~defproblem _599_T2_THM (in landau2)
 (conclusion conc _599_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREISE _MOREE _SATZ65B _PICP _MOREI)

"))

(th~defproblem _SATZ99B_THM (in landau2)
 (conclusion conc _SATZ99B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_599_T2 _RATAPP4)

"))

(th~defproblem _SATZ99C_THM (in landau2)
 (conclusion conc _SATZ99C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ85 _SATZ99A _SATZ82)

"))

(th~defproblem _SATZ99D_THM (in landau2)
 (conclusion conc _SATZ99D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ85 _SATZ83 _SATZ99B _SATZ82)

"))

(th~defproblem _5100_T1_THM (in landau2)
 (conclusion conc _5100_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREISE _SATZ66 _PICP _MOREISI)

"))

(th~defproblem _SATZ100_THM (in landau2)
 (conclusion conc _SATZ100)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5100_T1 _RATAPP4)

"))

(th~defproblem _SATZ100A_THM (in landau2)
 (conclusion conc _SATZ100A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ85 _SATZ100 _SATZ84)

"))

(th~defproblem _5101_T1_THM (in landau2)
 (conclusion conc _5101_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ94 _RT_ISMORE1)

"))

(th~defproblem _5101_T2_THM (in landau2)
 (conclusion conc _5101_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5101_T1 _SATZ81D _TH3)

"))

(th~defproblem _VORBEMERKUNG101_THM (in landau2)
 (conclusion conc _VORBEMERKUNG101)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5101_T2 _SOME_TH5)

"))

(th~defproblem _5101_T3_THM (in landau2)
 (conclusion conc _5101_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASS _PICP _ISI)

"))

(th~defproblem _5101_T4_THM (in landau2)
 (conclusion conc _5101_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5101_T3 _SOMEI)

"))

(th~defproblem _5101_T5_THM (in landau2)
 (conclusion conc _5101_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5101_T4 _MOREE _SATZ67A _SOMEAPP)

"))

(th~defproblem _SATZ101A_THM (in landau2)
 (conclusion conc _SATZ101A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5101_T5 _RATAPP2)

"))

(th~defproblem _5101_T6_THM (in landau2)
 (conclusion conc _5101_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PICP _ISE _SATZ67B _ISI)

"))

(th~defproblem _SATZ101B_THM (in landau2)
 (conclusion conc _SATZ101B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5101_T6 _RATAPP4)

"))

(th~defproblem _5101_T7_THM (in landau2)
 (conclusion conc _5101_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101B)

"))

(th~defproblem _SATZ101_THM (in landau2)
 (conclusion conc _SATZ101)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101A _5101_T7 _ONEI)

"))

(th~defproblem _SATZ101C_THM (in landau2)
 (conclusion conc _SATZ101C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101)

"))

(th~defproblem _SATZ101D_THM (in landau2)
 (conclusion conc _SATZ101D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101C _SYMIS)

"))

(th~defproblem _SATZ101E_THM (in landau2)
 (conclusion conc _SATZ101E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101C _RT_COMPL _TRIS)

"))

(th~defproblem _SATZ101F_THM (in landau2)
 (conclusion conc _SATZ101F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101E _SYMIS)

"))

(th~defproblem _SATZ101G_THM (in landau2)
 (conclusion conc _SATZ101G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ101C _SATZ101B)

"))

(th~defproblem _II5_T20_THM (in landau2)
 (conclusion conc _II5_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ68 _INCLASS _ISI)

"))

(th~defproblem _FTIMESFRT_THM (in landau2)
 (conclusion conc _FTIMESFRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T20)

"))

(th~defproblem _II5_T21_THM (in landau2)
 (conclusion conc _II5_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_FTIMESFRT _ISINDRAT)

"))

(th~defproblem _TICT_THM (in landau2)
 (conclusion conc _TICT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T21 _INCLASS _ISP)

"))

(th~defproblem _RT_ISTS1_THM (in landau2)
 (conclusion conc _RT_ISTS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RT_ISTS2_THM (in landau2)
 (conclusion conc _RT_ISTS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _RT_ISTS12_THM (in landau2)
 (conclusion conc _RT_ISTS12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISTS2 _RT_ISTS1 _TRIS)

"))

(th~defproblem _5102_T1_THM (in landau2)
 (conclusion conc _5102_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ69 _TICT _ISI)

"))

(th~defproblem _SATZ102_THM (in landau2)
 (conclusion conc _SATZ102)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5102_T1 _RATAPP2)

"))

(th~defproblem _RT_COMTS_THM (in landau2)
 (conclusion conc _RT_COMTS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ102)

"))

(th~defproblem _5103_T1_THM (in landau2)
 (conclusion conc _5103_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TICT)

"))

(th~defproblem _5103_T2_THM (in landau2)
 (conclusion conc _5103_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TICT)

"))

(th~defproblem _5103_T3_THM (in landau2)
 (conclusion conc _5103_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ70 _5103_T2 _5103_T1 _ISI)

"))

(th~defproblem _SATZ103_THM (in landau2)
 (conclusion conc _SATZ103)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5103_T3 _RATAPP3)

"))

(th~defproblem _RT_ASSTS1_THM (in landau2)
 (conclusion conc _RT_ASSTS1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ103)

"))

(th~defproblem _RT_ASSTS2_THM (in landau2)
 (conclusion conc _RT_ASSTS2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ103 _SYMIS)

"))

(th~defproblem _5104_T1_THM (in landau2)
 (conclusion conc _5104_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_PICP _TICT)

"))

(th~defproblem _5104_T2_THM (in landau2)
 (conclusion conc _5104_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TICT _PICP)

"))

(th~defproblem _5104_T3_THM (in landau2)
 (conclusion conc _5104_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ71 _5104_T2 _5104_T1 _ISI)

"))

(th~defproblem _SATZ104_THM (in landau2)
 (conclusion conc _SATZ104)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5104_T3 _RATAPP3)

"))

(th~defproblem _RT_DISTTP1_THM (in landau2)
 (conclusion conc _RT_DISTTP1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_COMTS _RT_ISPL12 _SATZ104 _TR3IS)

"))

(th~defproblem _RT_DISTTP2_THM (in landau2)
 (conclusion conc _RT_DISTTP2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ104)

"))

(th~defproblem _RT_DISTPT1_THM (in landau2)
 (conclusion conc _RT_DISTPT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_DISTTP1 _SYMIS)

"))

(th~defproblem _RT_DISTPT2_THM (in landau2)
 (conclusion conc _RT_DISTPT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_DISTTP2 _SYMIS)

"))

(th~defproblem _5105_T1_THM (in landau2)
 (conclusion conc _5105_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _SATZ72A _TICT _MOREI)

"))

(th~defproblem _SATZ105A_THM (in landau2)
 (conclusion conc _SATZ105A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5105_T1 _RATAPP3)

"))

(th~defproblem _5105_T2_THM (in landau2)
 (conclusion conc _5105_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISE _SATZ72B _TICT _ISI)

"))

(th~defproblem _SATZ105B_THM (in landau2)
 (conclusion conc _SATZ105B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5105_T2 _RATAPP3)

"))

(th~defproblem _5105_T3_THM (in landau2)
 (conclusion conc _5105_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_LESSE _SATZ72C _TICT _LESSI)

"))

(th~defproblem _SATZ105C_THM (in landau2)
 (conclusion conc _SATZ105C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5105_T3 _RATAPP3)

"))

(th~defproblem _5105_ANDERSB_THM (in landau2)
 (conclusion conc _5105_ANDERSB)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISTS1)

"))

(th~defproblem _5105_ANDERSC_THM (in landau2)
 (conclusion conc _5105_ANDERSC)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ105A _SATZ82)

"))

(th~defproblem _SATZ105D_THM (in landau2)
 (conclusion conc _SATZ105D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ105A _RT_COMTS _RT_ISMORE12)

"))

(th~defproblem _SATZ105E_THM (in landau2)
 (conclusion conc _SATZ105E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISTS2)

"))

(th~defproblem _SATZ105F_THM (in landau2)
 (conclusion conc _SATZ105F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ105C _RT_COMTS _RT_ISLESS12)

"))

(th~defproblem _5106_T1_THM (in landau2)
 (conclusion conc _5106_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TICT _MOREE _SATZ73A _MOREI)

"))

(th~defproblem _SATZ106A_THM (in landau2)
 (conclusion conc _SATZ106A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5106_T1 _RATAPP3)

"))

(th~defproblem _5106_T2_THM (in landau2)
 (conclusion conc _5106_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TICT _ISE _SATZ73B _ISI)

"))

(th~defproblem _SATZ106B_THM (in landau2)
 (conclusion conc _SATZ106B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5106_T2 _RATAPP3)

"))

(th~defproblem _5106_T3_THM (in landau2)
 (conclusion conc _5106_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TICT _LESSE _SATZ73C _LESSI)

"))

(th~defproblem _SATZ106C_THM (in landau2)
 (conclusion conc _SATZ106C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5106_T3 _RATAPP3)

"))

(th~defproblem _5106_ANDERS_THM (in landau2)
 (conclusion conc _5106_ANDERS)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ106A _SATZ82)

"))

(th~defproblem _5107_T1_THM (in landau2)
 (conclusion conc _5107_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _SATZ74 _TICT _MOREI)

"))

(th~defproblem _SATZ107_THM (in landau2)
 (conclusion conc _SATZ107)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5107_T1 _RATAPP4)

"))

(th~defproblem _SATZ107A_THM (in landau2)
 (conclusion conc _SATZ107A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ107 _SATZ82)

"))

(th~defproblem _5108_T1_THM (in landau2)
 (conclusion conc _5108_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREE _MOREISE _SATZ75A _TICT _MOREI)

"))

(th~defproblem _SATZ108A_THM (in landau2)
 (conclusion conc _SATZ108A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5108_T1 _RATAPP4)

"))

(th~defproblem _5108_T2_THM (in landau2)
 (conclusion conc _5108_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREISE _MOREE _SATZ75B _TICT _MOREI)

"))

(th~defproblem _SATZ108B_THM (in landau2)
 (conclusion conc _SATZ108B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5108_T2 _RATAPP4)

"))

(th~defproblem _SATZ108C_THM (in landau2)
 (conclusion conc _SATZ108C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ83 _SATZ85 _SATZ108A _SATZ82)

"))

(th~defproblem _SATZ108D_THM (in landau2)
 (conclusion conc _SATZ108D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ85 _SATZ83 _SATZ108B _SATZ82)

"))

(th~defproblem _5109_T1_THM (in landau2)
 (conclusion conc _5109_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_MOREISE _SATZ76 _TICT _MOREISI)

"))

(th~defproblem _SATZ109_THM (in landau2)
 (conclusion conc _SATZ109)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5109_T1 _RATAPP4)

"))

(th~defproblem _SATZ109A_THM (in landau2)
 (conclusion conc _SATZ109A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ85 _SATZ109 _SATZ84)

"))

(th~defproblem _5110_T1_THM (in landau2)
 (conclusion conc _5110_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASS _TICT _ISI)

"))

(th~defproblem _5110_T2_THM (in landau2)
 (conclusion conc _5110_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5110_T1 _SOMEI)

"))

(th~defproblem _5110_T3_THM (in landau2)
 (conclusion conc _5110_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5110_T2 _SATZ77A _SOMEAPP)

"))

(th~defproblem _SATZ110A_THM (in landau2)
 (conclusion conc _SATZ110A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5110_T3 _RATAPP2)

"))

(th~defproblem _5110_T4_THM (in landau2)
 (conclusion conc _5110_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_TICT _ISE _SATZ77B _ISI)

"))

(th~defproblem _SATZ110B_THM (in landau2)
 (conclusion conc _SATZ110B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5110_T4 _RATAPP4)

"))

(th~defproblem _5110_T5_THM (in landau2)
 (conclusion conc _5110_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110B)

"))

(th~defproblem _SATZ110_THM (in landau2)
 (conclusion conc _SATZ110)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110A _5110_T5 _ONEI)

"))

(th~defproblem _5111_T1_THM (in landau2)
 (conclusion conc _5111_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_SATZ28A _NDIS12 _TRIS)

"))

(th~defproblem _5111_T2_THM (in landau2)
 (conclusion conc _5111_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_5111_T1 _SYMIS)

"))

(th~defproblem _SATZ111A_THM (in landau2)
 (conclusion conc _SATZ111A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_5111_T1 _ISMORE12)

"))

(th~defproblem _SATZ111B_THM (in landau2)
 (conclusion conc _SATZ111B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_5111_T1 _5111_T2 _TR3IS)

"))

(th~defproblem _SATZ111C_THM (in landau2)
 (conclusion conc _SATZ111C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_5111_T1 _ISLESS12)

"))

(th~defproblem _SATZ111D_THM (in landau2)
 (conclusion conc _SATZ111D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_5111_T2 _ISMORE12)

"))

(th~defproblem _SATZ111E_THM (in landau2)
 (conclusion conc _SATZ111E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_5111_T2 _5111_T1 _TR3IS)

"))

(th~defproblem _SATZ111F_THM (in landau2)
 (conclusion conc _SATZ111F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX5)
Should be provable from hyps (_5111_T2 _ISLESS12)

"))

(th~defproblem _II5_T22_THM (in landau2)
 (conclusion conc _II5_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISE _SATZ111B)

"))

(th~defproblem _II5_T23_THM (in landau2)
 (conclusion conc _II5_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _II5_T22)

"))

(th~defproblem _SATZ111G_THM (in landau2)
 (conclusion conc _SATZ111G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T23 _ONEI)

"))

(th~defproblem _INCLASSN_THM (in landau2)
 (conclusion conc _INCLASSN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ111G)

"))

(th~defproblem _ISRTEN_THM (in landau2)
 (conclusion conc _ISRTEN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASSN _II5_T22)

"))

(th~defproblem _ISRTIN_THM (in landau2)
 (conclusion conc _ISRTIN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EQN _INCLASSN _ISI)

"))

(th~defproblem _NATRTI_THM (in landau2)
 (conclusion conc _NATRTI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASS _SOMEI)

"))

(th~defproblem _ISNERT_THM (in landau2)
 (conclusion conc _ISNERT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISF)

"))

(th~defproblem _ISNIRT_THM (in landau2)
 (conclusion conc _ISNIRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASS _II5_T22)

"))

(th~defproblem _ISRTN1_THM (in landau2)
 (conclusion conc _ISRTN1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _INCLASS _INCLASSN _ISI)

"))

(th~defproblem _ISNRT1_THM (in landau2)
 (conclusion conc _ISNRT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFIS _NATRTI _INCLASSN _INCLASS _II5_T22)

"))

(th~defproblem _SATZ112A_THM (in landau2)
 (conclusion conc _SATZ112A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ57)

"))

(th~defproblem _SATZ112B_THM (in landau2)
 (conclusion conc _SATZ112B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ28A _EQD _TFEQ12A _TREQ)

"))

(th~defproblem _SATZ112C_THM (in landau2)
 (conclusion conc _SATZ112C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ112A _INCLASSN _PICP _LEMMAEQ1)

"))

(th~defproblem _SATZ112D_THM (in landau2)
 (conclusion conc _SATZ112D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ112C _SOMEI)

"))

(th~defproblem _SATZ112E_THM (in landau2)
 (conclusion conc _SATZ112E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ112B _INCLASSN _TICT _LEMMAEQ1)

"))

(th~defproblem _SATZ112F_THM (in landau2)
 (conclusion conc _SATZ112F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ112E _SOMEI)

"))

(th~defproblem _5112_T1_THM (in landau2)
 (conclusion conc _5112_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INCLASSN _MOREE _SATZ111A)

"))

(th~defproblem _5112_T2_THM (in landau2)
 (conclusion conc _5112_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNRT1 _NATRTI _ISPL2 _TRIS)

"))

(th~defproblem _5112_T3_THM (in landau2)
 (conclusion conc _5112_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5112_T2 _NATRTI _EQN _SATZ112C _INCLASSN _ISI)

"))

(th~defproblem _5112_T4_THM (in landau2)
 (conclusion conc _5112_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5112_T3 _SYMIS _SATZ101G)

"))

(th~defproblem _5112_T5_THM (in landau2)
 (conclusion conc _5112_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5112_T4 _NATRTI _ISP)

"))

(th~defproblem _SATZ112G_THM (in landau2)
 (conclusion conc _SATZ112G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5112_T5 _5112_T1 _SOMEAPP)

"))

(th~defproblem _SATZ112H_THM (in landau2)
 (conclusion conc _SATZ112H)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ112A _INCLASS _PICP _ISI)

"))

(th~defproblem _SATZ112J_THM (in landau2)
 (conclusion conc _SATZ112J)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ112B _INCLASS _TICT _ISI)

"))

(th~defproblem _NT_NATRTI_THM (in landau2)
 (conclusion conc _NT_NATRTI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_INP)

"))

(th~defproblem _ISRTENT_THM (in landau2)
 (conclusion conc _ISRTENT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTI)

"))

(th~defproblem _ISRTINT_THM (in landau2)
 (conclusion conc _ISRTINT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTE)

"))

(th~defproblem _ISNTERT_THM (in landau2)
 (conclusion conc _ISNTERT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINI)

"))

(th~defproblem _ISNTIRT_THM (in landau2)
 (conclusion conc _ISNTIRT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINE)

"))

(th~defproblem _ISRTNT1_THM (in landau2)
 (conclusion conc _ISRTNT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISINOUT)

"))

(th~defproblem _ISNTRT1_THM (in landau2)
 (conclusion conc _ISNTRT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISOUTIN _NT_NATRTI)

"))

(th~defproblem _ISNENT_THM (in landau2)
 (conclusion conc _ISNENT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNERT _NATRTI _ISRTENT)

"))

(th~defproblem _ISNINT_THM (in landau2)
 (conclusion conc _ISNINT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NATRTI _ISRTINT _ISNIRT)

"))

(th~defproblem _ISNTEN_THM (in landau2)
 (conclusion conc _ISNTEN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTERT _NT_NATRTI _ISRTEN)

"))

(th~defproblem _ISNTIN_THM (in landau2)
 (conclusion conc _ISNTIN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _ISRTIN _ISNTIRT)

"))

(th~defproblem _II5_T24_THM (in landau2)
 (conclusion conc _II5_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NATRTI _ISRTNT1)

"))

(th~defproblem _II5_T25_THM (in landau2)
 (conclusion conc _II5_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T24 _NT_NATRTI _NATRTI _ISRTEN)

"))

(th~defproblem _ISNNT1_THM (in landau2)
 (conclusion conc _ISNNT1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T25 _ISNRT1 _NATRTI _TRIS)

"))

(th~defproblem _II5_T26_THM (in landau2)
 (conclusion conc _II5_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _ISRTN1)

"))

(th~defproblem _II5_T27_THM (in landau2)
 (conclusion conc _II5_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T26 _NATRTI _NT_NATRTI _ISRTENT)

"))

(th~defproblem _ISNTN1_THM (in landau2)
 (conclusion conc _ISNTN1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T27 _ISNTRT1 _NT_NATRTI _TRIS)

"))

(th~defproblem _ISNNT2_THM (in landau2)
 (conclusion conc _ISNNT2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT1 _SYMIS)

"))

(th~defproblem _ISNTN2_THM (in landau2)
 (conclusion conc _ISNTN2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTN1 _SYMIS)

"))

(th~defproblem _5113_T1_THM (in landau2)
 (conclusion conc _5113_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNINT)

"))

(th~defproblem _SATZ113A_THM (in landau2)
 (conclusion conc _SATZ113A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5113_T1 _TH3)

"))

(th~defproblem _5113_T2_THM (in landau2)
 (conclusion conc _5113_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNINT)

"))

(th~defproblem _SATZ113B_THM (in landau2)
 (conclusion conc _SATZ113B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5113_T2 _ISNTIN)

"))

(th~defproblem _5113_T3_THM (in landau2)
 (conclusion conc _5113_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.

"))

(th~defproblem _5113_T4_THM (in landau2)
 (conclusion conc _5113_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5113_T3 _MP)

"))

(th~defproblem _5113_T5_THM (in landau2)
 (conclusion conc _5113_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT2 _5113_T4 _ISP)

"))

(th~defproblem _5113_T6_THM (in landau2)
 (conclusion conc _5113_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5113_T5 _INDUCTION)

"))

(th~defproblem _SATZ113C_THM (in landau2)
 (conclusion conc _SATZ113C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTN2 _5113_T6 _ISP)

"))

(th~defproblem _AX3T_THM (in landau2)
 (conclusion conc _AX3T)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ113A)

"))

(th~defproblem _AX4T_THM (in landau2)
 (conclusion conc _AX4T)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ113B)

"))

(th~defproblem _AX5T_THM (in landau2)
 (conclusion conc _AX5T)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ113C)

"))

(th~defproblem _51_T1_THM (in landau2)
 (conclusion conc _51_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTIN _TH3)

"))

(th~defproblem _51_T2_THM (in landau2)
 (conclusion conc _51_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_51_T1 _SATZ1)

"))

(th~defproblem _NT_SATZ1_THM (in landau2)
 (conclusion conc _NT_SATZ1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNINT _51_T2 _TH3)

"))

(th~defproblem _54_T1_THM (in landau2)
 (conclusion conc _54_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _54_T2_THM (in landau2)
 (conclusion conc _54_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT2 _54_T1 _ISNTEN _TRIS)

"))

(th~defproblem _54_T3_THM (in landau2)
 (conclusion conc _54_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _54_T4_THM (in landau2)
 (conclusion conc _54_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT1 _ISF)

"))

(th~defproblem _54_T5_THM (in landau2)
 (conclusion conc _54_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T4 _ISF)

"))

(th~defproblem _54_T6_THM (in landau2)
 (conclusion conc _54_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T3)

"))

(th~defproblem _54_T7_THM (in landau2)
 (conclusion conc _54_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T6 _ISNTEN)

"))

(th~defproblem _54_T8_THM (in landau2)
 (conclusion conc _54_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT2)

"))

(th~defproblem _54_T9_THM (in landau2)
 (conclusion conc _54_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T8 _54_T7 _54_T5 _TR3IS)

"))

(th~defproblem _54_T10_THM (in landau2)
 (conclusion conc _54_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T9)

"))

(th~defproblem _54_T11_THM (in landau2)
 (conclusion conc _54_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T10 _54_T2 _ANDI)

"))

(th~defproblem _54_T12_THM (in landau2)
 (conclusion conc _54_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ4 _ONEE1)

"))

(th~defproblem _54_T13_THM (in landau2)
 (conclusion conc _54_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T11 _54_T12)

"))

(th~defproblem _54_T14_THM (in landau2)
 (conclusion conc _54_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T13 _FISE)

"))

(th~defproblem _54_T15_THM (in landau2)
 (conclusion conc _54_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T14 _ISNTIN)

"))

(th~defproblem _54_T16_THM (in landau2)
 (conclusion conc _54_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTN2 _ISF _54_T15 _ISNTN1 _TR3IS)

"))

(th~defproblem _54_T17_THM (in landau2)
 (conclusion conc _54_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T16 _FISI)

"))

(th~defproblem _54_T18_THM (in landau2)
 (conclusion conc _54_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T17)

"))

(th~defproblem _54_T19_THM (in landau2)
 (conclusion conc _54_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ4 _ONEE2)

"))

(th~defproblem _54_T20_THM (in landau2)
 (conclusion conc _54_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _54_T21_THM (in landau2)
 (conclusion conc _54_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT2 _ISF)

"))

(th~defproblem _54_T22_THM (in landau2)
 (conclusion conc _54_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T20 _54_T21 _TRIS _ISNENT)

"))

(th~defproblem _54_T23_THM (in landau2)
 (conclusion conc _54_T23)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _54_T24_THM (in landau2)
 (conclusion conc _54_T24)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT2 _ISF)

"))

(th~defproblem _54_T25_THM (in landau2)
 (conclusion conc _54_T25)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T23)

"))

(th~defproblem _54_T26_THM (in landau2)
 (conclusion conc _54_T26)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT1 _ISF)

"))

(th~defproblem _54_T27_THM (in landau2)
 (conclusion conc _54_T27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T26 _54_T25 _54_T24 _TR3IS _ISNENT)

"))

(th~defproblem _54_T28_THM (in landau2)
 (conclusion conc _54_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T27)

"))

(th~defproblem _54_T29_THM (in landau2)
 (conclusion conc _54_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T28 _54_T22 _ANDI)

"))

(th~defproblem _54_T30_THM (in landau2)
 (conclusion conc _54_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T29 _SOMEI)

"))

(th~defproblem _54_T31_THM (in landau2)
 (conclusion conc _54_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T30 _54_T19 _SOMEAPP)

"))

(th~defproblem _NT_SATZ4_THM (in landau2)
 (conclusion conc _NT_SATZ4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_54_T31 _54_T18 _ONEI)

"))

(th~defproblem _II5_T28_THM (in landau2)
 (conclusion conc _II5_T28)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _SATZ112C)

"))

(th~defproblem _II5_T29_THM (in landau2)
 (conclusion conc _II5_T29)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_REFEQ _INCLASS _II5_T28 _ISI)

"))

(th~defproblem _ISPLNT_THM (in landau2)
 (conclusion conc _ISPLNT)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T29 _NATRTI _NT_NATRTI _SATZ112D _ISRTENT)

"))

(th~defproblem _ISNTPL_THM (in landau2)
 (conclusion conc _ISNTPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISPLNT _SYMIS)

"))

(th~defproblem _ISPLN_THM (in landau2)
 (conclusion conc _ISPLN)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTPL _ISNTEN _ISNNT1 _TRIS)

"))

(th~defproblem _ISNPL_THM (in landau2)
 (conclusion conc _ISNPL)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISPLN _SYMIS)

"))

(th~defproblem _55_T1_THM (in landau2)
 (conclusion conc _55_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNPL _ISPL1)

"))

(th~defproblem _55_T2_THM (in landau2)
 (conclusion conc _55_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISPLN _ISPL2)

"))

(th~defproblem _55_T3_THM (in landau2)
 (conclusion conc _55_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_55_T2 _SATZ5 _55_T1 _TR3IS)

"))

(th~defproblem _NT_SATZ5_THM (in landau2)
 (conclusion conc _NT_SATZ5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTPL _55_T3 _ISNENT _ISPLNT _TR3IS)

"))

(th~defproblem _DIFFPROPE_THM (in landau2)
 (conclusion conc _DIFFPROPE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNPL _ISNTEN _TRIS)

"))

(th~defproblem _II5_T30_THM (in landau2)
 (conclusion conc _II5_T30)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISPLN _TRIS)

"))

(th~defproblem _DIFFPROPI_THM (in landau2)
 (conclusion conc _DIFFPROPI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T30 _ISNTIN)

"))

(th~defproblem _59_T1_THM (in landau2)
 (conclusion conc _59_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTIN _OR3I1)

"))

(th~defproblem _59_T2_THM (in landau2)
 (conclusion conc _59_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNNT1 _ISP)

"))

(th~defproblem _59_T3_THM (in landau2)
 (conclusion conc _59_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T2 _DIFFPROPI _SOMEI)

"))

(th~defproblem _59_T4_THM (in landau2)
 (conclusion conc _59_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T3 _SOMEAPP)

"))

(th~defproblem _59_T5_THM (in landau2)
 (conclusion conc _59_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T4 _OR3I2)

"))

(th~defproblem _59_T6_THM (in landau2)
 (conclusion conc _59_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T4 _OR3I3)

"))

(th~defproblem _59_T7_THM (in landau2)
 (conclusion conc _59_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T6 _59_T5 _59_T1 _SATZ9A _OR3APP)

"))

(th~defproblem _59_T8_THM (in landau2)
 (conclusion conc _59_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTEN)

"))

(th~defproblem _59_T9_THM (in landau2)
 (conclusion conc _59_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_DIFFPROPE _SOMEI)

"))

(th~defproblem _59_T10_THM (in landau2)
 (conclusion conc _59_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T9 _SOMEAPP)

"))

(th~defproblem _59_T11_THM (in landau2)
 (conclusion conc _59_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T10)

"))

(th~defproblem _59_T12_THM (in landau2)
 (conclusion conc _59_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ9B)

"))

(th~defproblem _59_T13_THM (in landau2)
 (conclusion conc _59_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T8 _59_T12 _EC3E12)

"))

(th~defproblem _59_T14_THM (in landau2)
 (conclusion conc _59_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T10 _59_T13 _TH3)

"))

(th~defproblem _59_T15_THM (in landau2)
 (conclusion conc _59_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T14 _EC_TH1)

"))

(th~defproblem _59_T16_THM (in landau2)
 (conclusion conc _59_T16)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T10 _59_T12 _EC3E23)

"))

(th~defproblem _59_T17_THM (in landau2)
 (conclusion conc _59_T17)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T11 _59_T16 _TH3)

"))

(th~defproblem _59_T18_THM (in landau2)
 (conclusion conc _59_T18)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T17 _EC_TH1)

"))

(th~defproblem _59_T19_THM (in landau2)
 (conclusion conc _59_T19)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T11 _59_T12 _EC3E31)

"))

(th~defproblem _59_T20_THM (in landau2)
 (conclusion conc _59_T20)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T8 _59_T19 _TH3)

"))

(th~defproblem _59_T21_THM (in landau2)
 (conclusion conc _59_T21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T20 _EC_TH1)

"))

(th~defproblem _59_T22_THM (in landau2)
 (conclusion conc _59_T22)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T21 _59_T18 _59_T15 _EC3_TH6)

"))

(th~defproblem _NT_SATZ9_THM (in landau2)
 (conclusion conc _NT_SATZ9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_59_T22 _59_T7 _OREC3I)

"))

(th~defproblem _II5_T31_THM (in landau2)
 (conclusion conc _II5_T31)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _INCLASSN _MOREE)

"))

(th~defproblem _NT_MOREE_THM (in landau2)
 (conclusion conc _NT_MOREE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T31 _SATZ111A)

"))

(th~defproblem _II5_T32_THM (in landau2)
 (conclusion conc _II5_T32)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ111D)

"))

(th~defproblem _NT_MOREI_THM (in landau2)
 (conclusion conc _NT_MOREI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T32 _NT_NATRTI _INCLASSN _MOREI)

"))

(th~defproblem _II5_T33_THM (in landau2)
 (conclusion conc _II5_T33)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _INCLASSN _LESSE)

"))

(th~defproblem _NT_LESSE_THM (in landau2)
 (conclusion conc _NT_LESSE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T33 _SATZ111C)

"))

(th~defproblem _II5_T34_THM (in landau2)
 (conclusion conc _II5_T34)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ111F)

"))

(th~defproblem _NT_LESSI_THM (in landau2)
 (conclusion conc _NT_LESSI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T34 _NT_NATRTI _INCLASSN _LESSI)

"))

(th~defproblem _NT_MOREISE_THM (in landau2)
 (conclusion conc _NT_MOREISE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _ISRTEN _NT_MOREE _TH9)

"))

(th~defproblem _NT_MOREISI_THM (in landau2)
 (conclusion conc _NT_MOREISI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _ISRTIN _NT_MOREI _TH9)

"))

(th~defproblem _NT_LESSISE_THM (in landau2)
 (conclusion conc _NT_LESSISE)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _ISRTEN _NT_LESSE _TH9)

"))

(th~defproblem _NT_LESSISI_THM (in landau2)
 (conclusion conc _NT_LESSISI)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_NATRTI _ISRTIN _NT_LESSI _TH9)

"))

(th~defproblem _515_T1_THM (in landau2)
 (conclusion conc _515_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_LESSE _SATZ15)

"))

(th~defproblem _NT_SATZ15_THM (in landau2)
 (conclusion conc _NT_SATZ15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_515_T1 _NT_LESSI)

"))

(th~defproblem _521_T1_THM (in landau2)
 (conclusion conc _521_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NT_MOREE _SATZ21)

"))

(th~defproblem _521_T2_THM (in landau2)
 (conclusion conc _521_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_521_T1 _ISPLN _ISMORE12)

"))

(th~defproblem _NT_SATZ21_THM (in landau2)
 (conclusion conc _NT_SATZ21)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_521_T2 _NT_MOREI)

"))

(th~defproblem _527_T1_THM (in landau2)
 (conclusion conc _527_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ISNTN1 _ISP)

"))

(th~defproblem _527_T2_THM (in landau2)
 (conclusion conc _527_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T1 _SOMEI)

"))

(th~defproblem _527_T3_THM (in landau2)
 (conclusion conc _527_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T2 _SOMEAPP)

"))

(th~defproblem _527_T4_THM (in landau2)
 (conclusion conc _527_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T3 _SATZ27)

"))

(th~defproblem _527_T5_THM (in landau2)
 (conclusion conc _527_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE1)

"))

(th~defproblem _527_T6_THM (in landau2)
 (conclusion conc _527_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T5 _MP)

"))

(th~defproblem _527_T7_THM (in landau2)
 (conclusion conc _527_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T1 _527_T6)

"))

(th~defproblem _527_T8_THM (in landau2)
 (conclusion conc _527_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T7 _ISNNT1 _ISLESSIS1)

"))

(th~defproblem _527_T9_THM (in landau2)
 (conclusion conc _527_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T8 _NT_LESSISI)

"))

(th~defproblem _527_T10_THM (in landau2)
 (conclusion conc _527_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T9)

"))

(th~defproblem _527_T11_THM (in landau2)
 (conclusion conc _527_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T10)

"))

(th~defproblem _527_T12_THM (in landau2)
 (conclusion conc _527_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_ANDE2)

"))

(th~defproblem _527_T13_THM (in landau2)
 (conclusion conc _527_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T12 _527_T11 _ANDI)

"))

(th~defproblem _527_T14_THM (in landau2)
 (conclusion conc _527_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T13 _SOMEI)

"))

(th~defproblem _NT_SATZ27_THM (in landau2)
 (conclusion conc _NT_SATZ27)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_527_T14 _527_T4 _SOMEAPP)

"))

(th~defproblem _II5_T35_THM (in landau2)
 (conclusion conc _II5_T35)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_FRIS _REFEQ1 _SATZ28A _EQND _TFEQ1A _TR3EQ)

"))

(th~defproblem _II5_T36_THM (in landau2)
 (conclusion conc _II5_T36)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T35 _INCLASS _TICT _ISI)

"))

(th~defproblem _EXAMPLE1A_THM (in landau2)
 (conclusion conc _EXAMPLE1A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_II5_T36 _RATAPP1)

"))

(th~defproblem _EXAMPLE1B_THM (in landau2)
 (conclusion conc _EXAMPLE1B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1A _SYMIS)

"))

(th~defproblem _EXAMPLE1C_THM (in landau2)
 (conclusion conc _EXAMPLE1C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1A _RT_COMTS _TRIS)

"))

(th~defproblem _EXAMPLE1D_THM (in landau2)
 (conclusion conc _EXAMPLE1D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_EXAMPLE1C _SYMIS)

"))

(th~defproblem _5114_T1_THM (in landau2)
 (conclusion conc _5114_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ40C _COMTS _EQN _TFEQ2A _TR3EQ)

"))

(th~defproblem _SATZ114_THM (in landau2)
 (conclusion conc _SATZ114)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5114_T1 _INCLASS _TICT _ISI)

"))

(th~defproblem _SATZ114A_THM (in landau2)
 (conclusion conc _SATZ114A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_NUMIS _ISNERT _SATZ114 _ISDEN _RT_ISTS1 _TR3IS)

"))

(th~defproblem _SATZ110C_THM (in landau2)
 (conclusion conc _SATZ110C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110)

"))

(th~defproblem _SATZ110D_THM (in landau2)
 (conclusion conc _SATZ110D)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110C _SYMIS)

"))

(th~defproblem _SATZ110E_THM (in landau2)
 (conclusion conc _SATZ110E)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110C _RT_COMTS _TRIS)

"))

(th~defproblem _SATZ110F_THM (in landau2)
 (conclusion conc _SATZ110F)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110E _SYMIS)

"))

(th~defproblem _SATZ110G_THM (in landau2)
 (conclusion conc _SATZ110G)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110C _SATZ110B)

"))

(th~defproblem _SATZ114B_THM (in landau2)
 (conclusion conc _SATZ114B)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ110C _SATZ114A _SATZ110B)

"))

(th~defproblem _SATZ114C_THM (in landau2)
 (conclusion conc _SATZ114C)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ114B _SYMIS)

"))

(th~defproblem _5115_T1_THM (in landau2)
 (conclusion conc _5115_T1)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ89)

"))

(th~defproblem _5115_T2_THM (in landau2)
 (conclusion conc _5115_T2)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ114B _ISFR _REFEQ1 _INCLASS _ISI _TRIS _RT_ISMORE1)

"))

(th~defproblem _5115_T3_THM (in landau2)
 (conclusion conc _5115_T3)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ111E _SATZ111D _SATZ24 _TH9 _INCLASS _MOREISI)

"))

(th~defproblem _5115_T4_THM (in landau2)
 (conclusion conc _5115_T4)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ASSTS2 _SATZ110F _RT_ISTS2 _RT_COMTS _TR3IS)

"))

(th~defproblem _5115_T5_THM (in landau2)
 (conclusion conc _5115_T5)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_SATZ105D _5115_T4 _SYMIS _RT_ISMORE1)

"))

(th~defproblem _5115_T6_THM (in landau2)
 (conclusion conc _5115_T6)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T5 _RT_MOREISI1)

"))

(th~defproblem _5115_T7_THM (in landau2)
 (conclusion conc _5115_T7)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_RT_ISTS2 _5115_T4 _TRIS _RT_MOREISI2)

"))

(th~defproblem _5115_T8_THM (in landau2)
 (conclusion conc _5115_T8)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T7 _5115_T6 _5115_T3 _ORAPP)

"))

(th~defproblem _5115_T9_THM (in landau2)
 (conclusion conc _5115_T9)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T2 _SATZ105D _SATZ110C _EXAMPLE1B _RT_ISMORE12)

"))

(th~defproblem _5115_T10_THM (in landau2)
 (conclusion conc _5115_T10)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T9 _5115_T8 _SATZ87C)

"))

(th~defproblem _5115_T11_THM (in landau2)
 (conclusion conc _5115_T11)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T10 _SOMEI)

"))

(th~defproblem _5115_T12_THM (in landau2)
 (conclusion conc _5115_T12)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T11 _RATAPP1)

"))

(th~defproblem _SATZ115_THM (in landau2)
 (conclusion conc _SATZ115)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T12 _5115_T1 _SOMEAPP)

"))

(th~defproblem _5115_T13_THM (in landau2)
 (conclusion conc _5115_T13)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T10 _NATRTI _ANDI)

"))

(th~defproblem _5115_T14_THM (in landau2)
 (conclusion conc _5115_T14)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T13 _SOMEI)

"))

(th~defproblem _5115_T15_THM (in landau2)
 (conclusion conc _5115_T15)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T14 _RATAPP1)

"))

(th~defproblem _SATZ115A_THM (in landau2)
 (conclusion conc _SATZ115A)
 (help "Theorem from Jutting's translation of Landau's Grundlagen.
May depend on axioms (_ONEAX _AX3 _AX4 _AX5)
Should be provable from hyps (_5115_T15 _5115_T1 _SOMEAPP)

"))

(th~defproblem _12ISND_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _ISDEN _ISNUM) _ISTS12) _12ISND))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _1DISND_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISNUM _N_ISTS1) _1DISND))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _N2ISND_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISDEN _N_ISTS2) _N2ISND))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _REFEQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ37 _REFEQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQND_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISND _REFEQ1) _EQND))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQN_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISN _REFEQ1) _EQN))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQD_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISD _REFEQ1) _EQD))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SYMEQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ38 _SYMEQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _139_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ISTS12 _139_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _139_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _STETS _139_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ39_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _139_T4 _SATZ33B) _SATZ39))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TREQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ39 _TREQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TREQ1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SYMEQ _TREQ) _TREQ1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TREQ2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SYMEQ _TREQ) _TREQ2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TR3EQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _TREQ _TR3EQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TR4EQ_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TREQ _TR3EQ) _TR4EQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ40A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ40 _SYMEQ) _SATZ40A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ40C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ40B _SYMEQ) _SATZ40C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREFI12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _12ISND _ISMORE12) _MOREFI12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSFI12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _12ISND _ISLESS12) _LESSFI12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREFI1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _1DISND _N2ISND) _ISMORE12) _MOREFI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREFI2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _N2ISND _1DISND) _ISMORE12) _MOREFI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSFI1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _1DISND _N2ISND) _ISLESS12) _LESSFI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSFI2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _N2ISND _1DISND) _ISLESS12) _LESSFI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ10 _SATZ41))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41A_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ10A _SATZ41A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41B_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ10B _SATZ41B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ42_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ11 _SATZ42))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ43_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ12 _SATZ43))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _244_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SYMEQ _ISTS12) _244_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ44_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _244_T3 _COMTS) _ISMORE1) _SATZ33A) _SATZ44))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQMOREF12_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ44 _EQMOREF12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQMOREF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ44) _EQMOREF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQMOREF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ44) _EQMOREF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ45_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ43 _SATZ44) _SATZ42) _SATZ45))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQLESSF12_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ45 _EQLESSF12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQLESSF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ45) _EQLESSF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQLESSF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ45) _EQLESSF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREQI2_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI2 _MOREQI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSEQI2_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI2 _LESSEQI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREQI1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI1 _MOREQI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSEQI1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI1 _LESSEQI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMOR _SATZ41B) _EC3_TH7) _SATZ41C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ41B _EC3_TH9) _SATZ41D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41E_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ41A _OR3_TH2) _SATZ41E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ41A _OR3_TH3) _COMOR) _SATZ41F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41G_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ41B _EC3E21) _EC3E23) _OR_TH3) _SATZ41G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41H_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ41B _EC3E31) _EC3E32) _OR_TH3) _SATZ41H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41J_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH4 _OR_TH5) _SATZ41A) _OR3E3) _SATZ41J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ41K_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH5 _OR_TH4) _SATZ41A) _OR3E2) _SATZ41K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _246_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ44 _ORI1) _246_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _246_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SYMEQ _TR3EQ) _ORI2) _246_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ46_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _246_T2 _246_T1) _ORAPP) _SATZ46))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQMOREQ12_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ46 _EQMOREQ12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQMOREQ1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ46) _EQMOREQ1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQMOREQ2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ46) _EQMOREQ2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _247_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ45 _ORI1) _247_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _247_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SYMEQ _TR3EQ) _ORI2) _247_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ47_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _247_T2 _247_T1) _ORAPP) _SATZ47))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQLESSEQ12_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ47 _EQLESSEQ12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQLESSEQ1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ47) _EQLESSEQ1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQLESSEQ2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ47) _EQLESSEQ2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ48_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ38 _SATZ42) _TH9) _SATZ48))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ49_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ38 _SATZ43) _TH9) _SATZ49))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _250_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ34A _250_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ50_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _250_T2 _SATZ33C) _SATZ50))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRLESSF_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ50 _TRLESSF))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRMOREF_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ42 _SATZ50) _SATZ43) _TRMOREF))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ51A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SYMEQ _EQLESSF1) _SATZ50) _ORAPP) _SATZ51A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ51B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _EQLESSF2 _SATZ50) _ORAPP) _SATZ51B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ51C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ48 _SATZ42) _SATZ51B) _SATZ43) _SATZ51C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ51D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ42 _SATZ48) _SATZ51A) _SATZ43) _SATZ51D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _252_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TREQ _ORI2) _252_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _252_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ51A _ORI1) _252_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _252_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _252_T1 _252_T2) _ORAPP) _252_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _252_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ51B _ORI1) _252_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ52_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _252_T3 _252_T4) _ORAPP) _SATZ52))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRLESSEQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ52 _TRLESSEQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _252_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ51B _ORI1) _252_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _252_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SYMEQ _EQLESSEQ1) _252_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _252_ANDERS_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _252_T6 _252_T5) _ORAPP) _252_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TRMOREQ_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ48 _SATZ52) _SATZ49) _TRMOREQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _253_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ18 _DISTPT1) _ISMORE1) _253_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _253_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _253_T1 _MOREFI2) _253_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _254_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ18A _DISTPT2) _ISLESS2) _254_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _254_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _254_T1 _LESSFI2) _254_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _255_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ19F _255_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _255_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ19C _255_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _255_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _255_T1 _DISTPT1) _DISTPT2) _ISLESS12) _255_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _255_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _255_T3 _LESSFI1) _255_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _255_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _255_T2 _DISTPT2) _DISTPT1) _ISLESS12) _255_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _255_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _255_T5 _LESSFI2) _255_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _255_T7_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _255_T6 _255_T4) _ANDI) _255_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II3_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NDIS12 _ISPL12) _II3_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II3_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DENIS _ISTS12) _II3_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PF12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _II3_T2 _II3_T1) _ISND) _PF12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II3_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _NDIS1D _NDISN2) _ISPL12) _II3_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II3_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DENIS _N_ISTS2) _II3_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _II3_T4 _II3_T3) _ISND) _PF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II3_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _NDISN2 _NDIS1D) _ISPL12) _II3_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II3_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DENIS _N_ISTS1) _II3_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _II3_T6 _II3_T5) _ISND) _PF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PFEQ12A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _PF12 _REFEQ1) _PFEQ12A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PFEQ12B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _PF12 _REFEQ2) _PFEQ12B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PFEQ1A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _PF1 _REFEQ1) _PFEQ1A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PFEQ1B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _PF1 _REFEQ2) _PFEQ1B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PFEQ2A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _PF2 _REFEQ1) _PFEQ2A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _PFEQ2B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _PF2 _REFEQ2) _PFEQ2B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _356_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _N_ISTS1 _356_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _356_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _356_T1 _356_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _356_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _356_T5 _356_T4) _ISPL12) _356_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ56_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _356_T7 _EQI12) _SATZ56))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQPF12_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ56 _EQPF12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQPF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _EQPF12) _EQPF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQPF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _EQPF12) _EQPF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ57_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _SATZ40C _DISTPT1) _EQN) _PFEQ12A) _TR3EQ) _SATZ57))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ57A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ57 _SYMEQ) _SATZ57A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ58_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMTS _COMPL) _EQND) _SATZ58))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _COMPF_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ58 _COMPF))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _359_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _359_T1 _ASSTS1) _ISPL12) _359_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _359_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _359_T4 _359_T3) _ISPL12) _359_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _359_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DISTPT1 _ISPL2) _359_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ59_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _PFEQ1B _ASSTS1) _359_T7) _EQND) _PFEQ2A) _TR3EQ) _SATZ59))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ASSPF1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ59 _ASSPF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ASSPF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ASSPF1 _SYMEQ) _ASSPF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _360_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ18 _360_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _360_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _360_T1 _SATZ32A) _360_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _360_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _360_T2 _360_T3) _ISMORE2) _360_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ60_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _360_T4 _MOREFI2) _SATZ60))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ60A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ60 _SATZ42) _SATZ60A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _361_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ32A _361_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _361_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _361_T1 _STETS1) _ISMORE12) _361_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _361_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES _STETS1 _361_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _361_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _361_T2 _361_T3) _SATZ19H) _361_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _361_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _361_T4 _DISTPT1) _ISMORE12) _361_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _361_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _361_T5 _SATZ32A) _361_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _361_T7_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _361_T6 _ASSTS1) _ISMORE12) _361_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ61_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _361_T7 _MOREFI12) _SATZ61))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62A_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ61 _SATZ62A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62B_HTHM (in landau2)
 (conclusion conc (IMPLIES _EQPF1 _SATZ62B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ43 _SATZ61) _SATZ42) _SATZ62C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ62A _COMPF) _EQMOREF12) _SATZ62D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62E_HTHM (in landau2)
 (conclusion conc (IMPLIES _EQPF2 _SATZ62E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ62C _COMPF) _EQLESSF12) _SATZ62F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62G_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ62D _EQPF1) _EQMOREF2) _SATZ62G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62H_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ62G _COMPF) _EQMOREF12) _SATZ62H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62J_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ62F _EQPF1) _EQLESSF2) _SATZ62J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ62K_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ62J _COMPF) _EQLESSF12) _SATZ62K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _363_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ41A _363_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _363_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ41B _363_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ63A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ62C _SATZ62A) _SATZ62B) _363_T2) _363_T1) _EC3_TH11) _SATZ63A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ63B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ62C _SATZ62A) _SATZ62B) _363_T2) _363_T1) _EC3_TH10) _SATZ63B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ63C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ62C _SATZ62A) _SATZ62B) _363_T2) _363_T1) _EC3_TH12) _SATZ63C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ63D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMPF _EQMOREF12) _SATZ63A) _SATZ63D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ63E_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMPF _TR3EQ) _SATZ63B) _SATZ63E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ63F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMPF _EQLESSF12) _SATZ63C) _SATZ63F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _364_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ61 _364_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _364_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ61 _COMPF) _EQMOREF12) _364_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ64_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _364_T2 _364_T1) _TRMOREF) _SATZ64))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ64A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ43 _SATZ64) _SATZ42) _SATZ64A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ65A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ62G _SATZ64) _ORAPP) _SATZ65A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ65B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _SATZ61 _EQPF2) _EQMOREF2) _SATZ64) _ORAPP) _SATZ65B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ65C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ43 _SATZ49) _SATZ65A) _SATZ42) _SATZ65C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ65D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ49 _SATZ43) _SATZ65B) _SATZ42) _SATZ65D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _366_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ56 _MOREQI2) _366_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _366_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ65A _MOREQI1) _366_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _366_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _366_T1 _366_T2) _ORAPP) _366_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _366_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ65B _MOREQI1) _366_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ66_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _366_T3 _366_T4) _ORAPP) _SATZ66))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ66A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ49 _SATZ66) _SATZ48) _SATZ66A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _367_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ60 _EQMOREF1) _367_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _367_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _367_T1 _SATZ41D) _TH3) _367_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ67B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TREQ2 _SATZ63E) _SATZ67B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _367_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES _367_T3 _367_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _367_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _COMTS _EQD) _SATZ40) _TREQ) _367_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ67C_HTHM (in landau2)
 (conclusion conc (IMPLIES _367_T6 _SATZ67C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ67D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ67C _SYMEQ) _SATZ67D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ67E_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ67C _SATZ67B) _SATZ67E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II4_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NUMIS _ISTS12) _II4_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II4_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DENIS _ISTS12) _II4_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TF12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _II4_T2 _II4_T1) _ISND) _TF12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II4_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NUMIS _N_ISTS2) _II4_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II4_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DENIS _N_ISTS2) _II4_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _II4_T4 _II4_T3) _ISND) _TF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II4_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NUMIS _N_ISTS1) _II4_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II4_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DENIS _N_ISTS1) _II4_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _II4_T6 _II4_T5) _ISND) _TF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TFEQ12A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TF12 _REFEQ1) _TFEQ12A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TFEQ12B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TF12 _REFEQ2) _TFEQ12B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TFEQ1A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TF1 _REFEQ1) _TFEQ1A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TFEQ1B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TF1 _REFEQ2) _TFEQ1B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TFEQ2A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TF2 _REFEQ1) _TFEQ2A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _TFEQ2B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TF2 _REFEQ2) _TFEQ2B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _468_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ISTS12 _468_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ68_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _468_T2 _EQI12) _SATZ68))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQTF12_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ68 _EQTF12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQTF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _EQTF12) _EQTF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EQTF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _EQTF12) _EQTF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ69_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _COMTS _EQND) _SATZ69))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _COMTF_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ69 _COMTF))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ70_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _TFEQ1B _ASSTS1) _EQND) _TFEQ2A) _TR3EQ) _SATZ70))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ASSTF1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ70 _ASSTF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ASSTF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ASSTF1 _SYMEQ) _ASSTF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _471_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _SATZ57A _DISTTP2) _EQN) _TFEQ1A) _TR3EQ) _471_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _471_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ40C _ASSTS2) _EQND) _TREQ) _471_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _471_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _471_T2 _COMTS) _N_ISTS2) _EQD) _TREQ) _471_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ71_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _471_T3 _471_T2) _EQPF12) _471_T1) _TREQ) _SATZ71))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _DISTTPF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _COMTF _EQPF12) _SATZ71) _TR3EQ) _DISTTPF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _DISTTPF2_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ71 _DISTTPF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _DISTPTF1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DISTTPF1 _SYMEQ) _DISTPTF1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _DISTPTF2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _DISTTPF2 _SYMEQ) _DISTPTF2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _472_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ32A _472_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _472_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _472_T1 _STETS2) _ISMORE12) _472_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _472_T2 _MOREFI12) _SATZ72A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _REFEQ _SATZ68) _SATZ72B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ43 _SATZ72A) _SATZ42) _SATZ72C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72A _COMTF) _EQMOREF12) _SATZ72D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72E_HTHM (in landau2)
 (conclusion conc (IMPLIES _EQTF2 _SATZ72E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72C _COMTF) _EQLESSF12) _SATZ72F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72G_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72D _EQTF1) _EQMOREF2) _SATZ72G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72H_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72G _COMTF) _EQMOREF12) _SATZ72H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72J_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72F _EQTF1) _EQLESSF2) _SATZ72J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ72K_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72J _COMTF) _EQLESSF12) _SATZ72K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _473_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ41A _473_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _473_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ41B _473_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ73A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ72C _SATZ72A) _SATZ72B) _473_T2) _473_T1) _EC3_TH11) _SATZ73A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ73B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ72C _SATZ72A) _SATZ72B) _473_T2) _473_T1) _EC3_TH10) _SATZ73B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ73C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ72C _SATZ72A) _SATZ72B) _473_T2) _473_T1) _EC3_TH12) _SATZ73C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ73D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMTF _EQMOREF12) _SATZ73A) _SATZ73D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ73E_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMTF _TR3EQ) _SATZ73B) _SATZ73E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ73F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMTF _EQLESSF12) _SATZ73C) _SATZ73F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _474_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ72A _474_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _474_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72A _COMTF) _EQMOREF12) _474_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ74_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _474_T2 _474_T1) _TRMOREF) _SATZ74))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ74A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ43 _SATZ74) _SATZ42) _SATZ74A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ75A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ72G _SATZ74) _ORAPP) _SATZ75A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ75B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _SATZ72A _EQTF2) _EQMOREF2) _SATZ74) _ORAPP) _SATZ75B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ75C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ43 _SATZ49) _SATZ75A) _SATZ42) _SATZ75C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ75D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ49 _SATZ43) _SATZ75B) _SATZ42) _SATZ75D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _476_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ68 _MOREQI2) _476_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _476_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ75A _MOREQI1) _476_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _476_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _476_T1 _476_T2) _ORAPP) _476_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _476_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ75B _MOREQI1) _476_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ76_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _476_T3 _476_T4) _ORAPP) _SATZ76))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ76A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ49 _SATZ76) _SATZ48) _SATZ76A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ77B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TREQ2 _SATZ73E) _SATZ77B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_REFEQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _REFEQ _RT_REFEQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_SYMEQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _SYMEQ _RT_SYMEQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_TREQ_HTHM (in landau2)
 (conclusion conc (IMPLIES _TREQ _RT_TREQ))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _RATAPP1 _II5_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RATAPP2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _II5_T1 _RATAPP1) _RATAPP2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _RATAPP2 _II5_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RATAPP3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _II5_T2 _RATAPP1) _RATAPP3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES _RATAPP3 _II5_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RATAPP4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _II5_T3 _RATAPP1) _RATAPP4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NISI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISE _TH3) _NISI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NISE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISI _TH3) _NISE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3E1 _II5_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3E2 _II5_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3E3 _II5_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T9_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3I _II5_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREE_HTHM (in landau2)
 (conclusion conc (IMPLIES _ALSO18 _MOREE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_ISMORE12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _RT_ISMORE1 _RT_ISMORE2) _RT_ISMORE12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T11_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3E1 _II5_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T12_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3E2 _II5_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T13_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3E3 _II5_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T16_HTHM (in landau2)
 (conclusion conc (IMPLIES _AND3I _II5_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSE_HTHM (in landau2)
 (conclusion conc (IMPLIES _ALSO19 _LESSE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_ISLESS12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _RT_ISLESS1 _RT_ISLESS2) _RT_ISLESS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ41A _581_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISI _OR3I1) _581_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _MOREI _OR3I2) _581_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _LESSI _OR3I3) _581_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _581_T4 _581_T3) _581_T2) _581_T1) _OR3APP) _581_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ41B _581_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T7_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _MOREE _ISE) _581_T6) _EC3E12) _TH3) _581_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T8_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _LESSE _MOREE) _581_T6) _EC3E23) _TH3) _581_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T9_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _ISE _LESSE) _581_T6) _EC3E31) _TH3) _581_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T10_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _581_T9 _EC_TH1) _581_T8) _581_T7) _EC3_TH6) _581_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _581_T11_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _581_T10 _581_T5) _OREC3I) _581_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _581_T11 _RATAPP2) _SATZ81))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ81 _OREC3E1) _SATZ81A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ81 _OREC3E2) _SATZ81B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _582_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _MOREE _SATZ42) _LESSI) _582_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ82_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _582_T1 _RATAPP2) _SATZ82))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _583_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _LESSE _SATZ43) _MOREI) _583_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ83_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _583_T1 _RATAPP2) _SATZ83))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_MOREISI1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI1 _RT_MOREISI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_MOREISI2_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI2 _RT_MOREISI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREISI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _ISI _RT_MOREISI2) _MOREI) _RT_MOREISI1) _ORAPP) _MOREISI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _MOREISE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _ISE _MOREQI2) _MOREE) _MOREQI1) _ORAPP) _MOREISE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_ISMOREIS12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _RT_ISMOREIS1 _RT_ISMOREIS2) _RT_ISMOREIS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_LESSISI1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI1 _RT_LESSISI1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_LESSISI2_HTHM (in landau2)
 (conclusion conc (IMPLIES _ORI2 _RT_LESSISI2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSISI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _ISI _RT_LESSISI2) _LESSI) _RT_LESSISI1) _ORAPP) _LESSISI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _LESSISE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _ISE _LESSEQI2) _LESSE) _LESSEQI1) _ORAPP) _LESSISE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_ISLESSIS12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _RT_ISLESSIS1 _RT_ISLESSIS2) _RT_ISLESSIS12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _COMOR _SATZ81B) _EC3_TH7) _SATZ81C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ81B _EC3_TH9) _SATZ81D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81E_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ81A _OR3_TH2) _SATZ81E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ81A _OR3_TH3) _COMOR) _SATZ81F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81G_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ81B _EC3E21) _EC3E23) _OR_TH3) _SATZ81G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81H_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ81B _EC3E31) _EC3E32) _OR_TH3) _SATZ81H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81J_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH4 _OR_TH5) _SATZ81A) _OR3E3) _SATZ81J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ81K_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _OR_TH5 _OR_TH4) _SATZ81A) _OR3E2) _SATZ81K))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _584_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _MOREISE _SATZ48) _LESSISI) _584_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ84_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _584_T1 _RATAPP2) _SATZ84))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _585_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _LESSISE _SATZ49) _MOREISI) _585_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ85_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _585_T1 _RATAPP2) _SATZ85))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _586_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _LESSE _SATZ50) _LESSI) _586_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ86_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _586_T1 _RATAPP3) _SATZ86))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_TRLESS_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ86 _RT_TRLESS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_TRMORE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ82 _SATZ86) _SATZ83) _RT_TRMORE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _587_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _LESSE _LESSISE) _SATZ51A) _LESSI) _587_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ87A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _587_T1 _RATAPP3) _SATZ87A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _587_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _LESSISE _LESSE) _SATZ51B) _LESSI) _587_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ87B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _587_T2 _RATAPP3) _SATZ87B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ87C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ84 _SATZ82) _SATZ87B) _SATZ83) _SATZ87C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ87D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ82 _SATZ84) _SATZ87A) _SATZ83) _SATZ87D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _588_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _LESSISE _SATZ52) _LESSISI) _588_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ88_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _588_T1 _RATAPP3) _SATZ88))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_TRLESSIS_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ88 _RT_TRLESSIS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_TRMOREIS_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ84 _SATZ88) _SATZ85) _RT_TRMOREIS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ89_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _589_T2 _RATAPP1) _SATZ89))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ90_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _590_T2 _RATAPP1) _SATZ90))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _591_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _ANDE1 _INCLASS) _LESSI) _591_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _591_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _ANDE2 _INCLASS) _LESSI) _591_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _591_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _591_T2 _591_T1) _ANDI) _591_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ91_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _591_T5 _RATAPP2) _SATZ91))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T18_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ56 _INCLASS) _ISI) _II5_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _FPLUSFRT_HTHM (in landau2)
 (conclusion conc (IMPLIES _II5_T18 _FPLUSFRT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _592_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ58 _PICP) _ISI) _592_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ92_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _592_T1 _RATAPP2) _SATZ92))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_COMPL_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ92 _RT_COMPL))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _593_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _PICP _593_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _593_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _PICP _593_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _593_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ59 _593_T2) _593_T1) _ISI) _593_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ93_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _593_T3 _RATAPP3) _SATZ93))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_ASSPL1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ93 _RT_ASSPL1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _594_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ60 _PICP) _MOREI) _594_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ94_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _594_T1 _RATAPP2) _SATZ94))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ94A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ94 _SATZ82) _SATZ94A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _595_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _MOREE _SATZ61) _PICP) _MOREI) _595_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ95_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _595_T1 _RATAPP3) _SATZ95))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _596_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _MOREE _SATZ62A) _PICP) _MOREI) _596_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ96A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _596_T1 _RATAPP3) _SATZ96A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _596_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _ISE _SATZ62B) _PICP) _ISI) _596_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ96B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _596_T2 _RATAPP3) _SATZ96B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _596_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _LESSE _SATZ62C) _PICP) _LESSI) _596_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ96C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _596_T3 _RATAPP3) _SATZ96C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _596_ANDERSA_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ95 _596_ANDERSA))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _596_ANDERSB_HTHM (in landau2)
 (conclusion conc (IMPLIES _RT_ISPL1 _596_ANDERSB))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _596_ANDERSC_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ83 _SATZ95) _SATZ82) _596_ANDERSC))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ96D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ96A _RT_COMPL) _RT_ISMORE12) _SATZ96D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ96E_HTHM (in landau2)
 (conclusion conc (IMPLIES _RT_ISPL2 _SATZ96E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ96F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ96C _RT_COMPL) _RT_ISLESS12) _SATZ96F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _597_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _PICP _MOREE) _SATZ63A) _MOREI) _597_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ97A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _597_T1 _RATAPP3) _SATZ97A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _597_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _PICP _ISE) _SATZ63B) _ISI) _597_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ97B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _597_T2 _RATAPP3) _SATZ97B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _597_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _PICP _LESSE) _SATZ63C) _LESSI) _597_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ97C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _597_T3 _RATAPP3) _SATZ97C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _597_ANDERS_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ83 _SATZ97A) _SATZ82) _597_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _598_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _MOREE _SATZ64) _PICP) _MOREI) _598_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ98_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _598_T1 _RATAPP4) _SATZ98))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ98A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ83 _SATZ98) _SATZ82) _SATZ98A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _599_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _MOREE _MOREISE) _SATZ65A) _PICP) _MOREI) _599_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ99A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _599_T1 _RATAPP4) _SATZ99A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _599_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _MOREISE _MOREE) _SATZ65B) _PICP) _MOREI) _599_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ99B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _599_T2 _RATAPP4) _SATZ99B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ99C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ83 _SATZ85) _SATZ99A) _SATZ82) _SATZ99C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ99D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ85 _SATZ83) _SATZ99B) _SATZ82) _SATZ99D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5100_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _MOREISE _SATZ66) _PICP) _MOREISI) _5100_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ100_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5100_T1 _RATAPP4) _SATZ100))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ100A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ85 _SATZ100) _SATZ84) _SATZ100A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5101_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ94 _RT_ISMORE1) _5101_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5101_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _5101_T1 _SATZ81D) _TH3) _5101_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5101_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _INCLASS _PICP) _ISI) _5101_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ101A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5101_T5 _RATAPP2) _SATZ101A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5101_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _PICP _ISE) _SATZ67B) _ISI) _5101_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ101B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5101_T6 _RATAPP4) _SATZ101B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5101_T7_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ101B _5101_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ101C_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ101 _SATZ101C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ101G_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ101C _SATZ101B) _SATZ101G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T20_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ68 _INCLASS) _ISI) _II5_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _FTIMESFRT_HTHM (in landau2)
 (conclusion conc (IMPLIES _II5_T20 _FTIMESFRT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5102_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ69 _TICT) _ISI) _5102_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ102_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5102_T1 _RATAPP2) _SATZ102))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_COMTS_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ102 _RT_COMTS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5103_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _TICT _5103_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5103_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _TICT _5103_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5103_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ70 _5103_T2) _5103_T1) _ISI) _5103_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ103_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5103_T3 _RATAPP3) _SATZ103))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_ASSTS1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ103 _RT_ASSTS1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5104_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _PICP _TICT) _5104_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5104_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _TICT _PICP) _5104_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5104_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ71 _5104_T2) _5104_T1) _ISI) _5104_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ104_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5104_T3 _RATAPP3) _SATZ104))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _RT_DISTTP2_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ104 _RT_DISTTP2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5105_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _MOREE _SATZ72A) _TICT) _MOREI) _5105_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ105A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5105_T1 _RATAPP3) _SATZ105A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5105_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _ISE _SATZ72B) _TICT) _ISI) _5105_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ105B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5105_T2 _RATAPP3) _SATZ105B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5105_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _LESSE _SATZ72C) _TICT) _LESSI) _5105_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ105C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5105_T3 _RATAPP3) _SATZ105C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5105_ANDERSB_HTHM (in landau2)
 (conclusion conc (IMPLIES _RT_ISTS1 _5105_ANDERSB))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5105_ANDERSC_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ83 _SATZ105A) _SATZ82) _5105_ANDERSC))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ105D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ105A _RT_COMTS) _RT_ISMORE12) _SATZ105D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ105E_HTHM (in landau2)
 (conclusion conc (IMPLIES _RT_ISTS2 _SATZ105E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ105F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ105C _RT_COMTS) _RT_ISLESS12) _SATZ105F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5106_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _TICT _MOREE) _SATZ73A) _MOREI) _5106_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ106A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5106_T1 _RATAPP3) _SATZ106A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5106_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _TICT _ISE) _SATZ73B) _ISI) _5106_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ106B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5106_T2 _RATAPP3) _SATZ106B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5106_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _TICT _LESSE) _SATZ73C) _LESSI) _5106_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ106C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5106_T3 _RATAPP3) _SATZ106C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5106_ANDERS_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ83 _SATZ106A) _SATZ82) _5106_ANDERS))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5107_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _MOREE _SATZ74) _TICT) _MOREI) _5107_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ107_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5107_T1 _RATAPP4) _SATZ107))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ107A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ83 _SATZ107) _SATZ82) _SATZ107A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5108_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _MOREE _MOREISE) _SATZ75A) _TICT) _MOREI) _5108_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ108A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5108_T1 _RATAPP4) _SATZ108A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5108_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _MOREISE _MOREE) _SATZ75B) _TICT) _MOREI) _5108_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ108B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5108_T2 _RATAPP4) _SATZ108B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ108C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ83 _SATZ85) _SATZ108A) _SATZ82) _SATZ108C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ108D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ85 _SATZ83) _SATZ108B) _SATZ82) _SATZ108D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5109_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _MOREISE _SATZ76) _TICT) _MOREISI) _5109_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ109_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5109_T1 _RATAPP4) _SATZ109))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ109A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ85 _SATZ109) _SATZ84) _SATZ109A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5110_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _INCLASS _TICT) _ISI) _5110_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ110A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5110_T3 _RATAPP2) _SATZ110A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5110_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _TICT _ISE) _SATZ77B) _ISI) _5110_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ110B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5110_T4 _RATAPP4) _SATZ110B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5110_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ110B _5110_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ111A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5111_T1 _ISMORE12) _SATZ111A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ111C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5111_T1 _ISLESS12) _SATZ111C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ111D_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5111_T2 _ISMORE12) _SATZ111D))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ111F_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5111_T2 _ISLESS12) _SATZ111F))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T22_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISE _SATZ111B) _II5_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _INCLASSN_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ111G _INCLASSN))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTEN_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _INCLASSN _II5_T22) _ISRTEN))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTIN_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _EQN _INCLASSN) _ISI) _ISRTIN))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNIRT_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _INCLASS _II5_T22) _ISNIRT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISRTN1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _REFEQ _INCLASS) _INCLASSN) _ISI) _ISRTN1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ112A_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ57 _SATZ112A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ112B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ28A _EQD) _TFEQ12A) _TREQ) _SATZ112B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ112C_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ112A _INCLASSN) _PICP) _LEMMAEQ1) _SATZ112C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ112E_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ112B _INCLASSN) _TICT) _LEMMAEQ1) _SATZ112E))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5112_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _INCLASSN _MOREE) _SATZ111A) _5112_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5112_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _5112_T2 _NATRTI) _EQN) _SATZ112C) _INCLASSN) _ISI) _5112_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ112H_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ112A _INCLASS) _PICP) _ISI) _SATZ112H))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ112J_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _SATZ112B _INCLASS) _TICT) _ISI) _SATZ112J))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNENT_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _ISNERT _NATRTI) _ISRTENT) _ISNENT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNINT_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _NATRTI _ISRTINT) _ISNIRT) _ISNINT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNTEN_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _ISNTERT _NT_NATRTI) _ISRTEN) _ISNTEN))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISNTIN_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _NT_NATRTI _ISRTIN) _ISNTIRT) _ISNTIN))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T24_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NATRTI _ISRTNT1) _II5_T24))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T25_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _II5_T24 _NT_NATRTI) _NATRTI) _ISRTEN) _II5_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T26_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NT_NATRTI _ISRTN1) _II5_T26))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T27_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _II5_T26 _NATRTI) _NT_NATRTI) _ISRTENT) _II5_T27))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5113_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ISNINT _5113_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ113A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5113_T1 _TH3) _SATZ113A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5113_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES _ISNINT _5113_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ113B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5113_T2 _ISNTIN) _SATZ113B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5113_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5113_T3 _MP) _5113_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5113_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5113_T5 _INDUCTION) _5113_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _AX3T_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ113A _AX3T))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _AX4T_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ113B _AX4T))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _AX5T_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ113C _AX5T))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _51_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISNTIN _TH3) _51_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _51_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _51_T1 _SATZ1) _51_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_SATZ1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _ISNINT _51_T2) _TH3) _NT_SATZ1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _ANDE1 _54_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES _ANDE2 _54_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES _54_T3 _54_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T7_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _54_T6 _ISNTEN) _54_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T8_HTHM (in landau2)
 (conclusion conc (IMPLIES _ISNNT2 _54_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T10_HTHM (in landau2)
 (conclusion conc (IMPLIES _54_T9 _54_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T11_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _54_T10 _54_T2) _ANDI) _54_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T13_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _54_T11 _54_T12) _54_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T15_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _54_T14 _ISNTIN) _54_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T18_HTHM (in landau2)
 (conclusion conc (IMPLIES _54_T17 _54_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T20_HTHM (in landau2)
 (conclusion conc (IMPLIES _ANDE1 _54_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T23_HTHM (in landau2)
 (conclusion conc (IMPLIES _ANDE2 _54_T23))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T25_HTHM (in landau2)
 (conclusion conc (IMPLIES _54_T23 _54_T25))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T28_HTHM (in landau2)
 (conclusion conc (IMPLIES _54_T27 _54_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _54_T29_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _54_T28 _54_T22) _ANDI) _54_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T28_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NT_NATRTI _SATZ112C) _II5_T28))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T29_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _REFEQ _INCLASS) _II5_T28) _ISI) _II5_T29))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _ISPLNT_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _II5_T29 _NATRTI) _NT_NATRTI) _SATZ112D) _ISRTENT) _ISPLNT))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _55_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISNPL _ISPL1) _55_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _55_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISPLN _ISPL2) _55_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _DIFFPROPI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _II5_T30 _ISNTIN) _DIFFPROPI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _ISNTIN _OR3I1) _59_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _59_T4 _OR3I2) _59_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _59_T4 _OR3I3) _59_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T7_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _59_T6 _59_T5) _59_T1) _SATZ9A) _OR3APP) _59_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T8_HTHM (in landau2)
 (conclusion conc (IMPLIES _ISNTEN _59_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T11_HTHM (in landau2)
 (conclusion conc (IMPLIES _59_T10 _59_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T12_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ9B _59_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T13_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _59_T8 _59_T12) _EC3E12) _59_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T14_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _59_T10 _59_T13) _TH3) _59_T14))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T15_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _59_T14 _EC_TH1) _59_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T16_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _59_T10 _59_T12) _EC3E23) _59_T16))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T17_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _59_T11 _59_T16) _TH3) _59_T17))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T18_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _59_T17 _EC_TH1) _59_T18))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T19_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _59_T11 _59_T12) _EC3E31) _59_T19))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T20_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _59_T8 _59_T19) _TH3) _59_T20))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T21_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _59_T20 _EC_TH1) _59_T21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _59_T22_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _59_T21 _59_T18) _59_T15) _EC3_TH6) _59_T22))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_SATZ9_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _59_T22 _59_T7) _OREC3I) _NT_SATZ9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T31_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _NT_NATRTI _INCLASSN) _MOREE) _II5_T31))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_MOREE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _II5_T31 _SATZ111A) _NT_MOREE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T32_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ111D _II5_T32))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_MOREI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _II5_T32 _NT_NATRTI) _INCLASSN) _MOREI) _NT_MOREI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T33_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _NT_NATRTI _INCLASSN) _LESSE) _II5_T33))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_LESSE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _II5_T33 _SATZ111C) _NT_LESSE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T34_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ111F _II5_T34))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_LESSI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _II5_T34 _NT_NATRTI) _INCLASSN) _LESSI) _NT_LESSI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_MOREISE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _NT_NATRTI _ISRTEN) _NT_MOREE) _TH9) _NT_MOREISE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_MOREISI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _NT_NATRTI _ISRTIN) _NT_MOREI) _TH9) _NT_MOREISI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_LESSISE_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _NT_NATRTI _ISRTEN) _NT_LESSE) _TH9) _NT_LESSISE))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_LESSISI_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _NT_NATRTI _ISRTIN) _NT_LESSI) _TH9) _NT_LESSISI))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _515_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NT_LESSE _SATZ15) _515_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_SATZ15_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _515_T1 _NT_LESSI) _NT_SATZ15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _521_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _NT_MOREE _SATZ21) _521_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _521_T2_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _521_T1 _ISPLN) _ISMORE12) _521_T2))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _NT_SATZ21_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _521_T2 _NT_MOREI) _NT_SATZ21))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T4_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _527_T3 _SATZ27) _527_T4))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T5_HTHM (in landau2)
 (conclusion conc (IMPLIES _ANDE1 _527_T5))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _527_T5 _MP) _527_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T7_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _527_T1 _527_T6) _527_T7))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T8_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _527_T7 _ISNNT1) _ISLESSIS1) _527_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T9_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _527_T8 _NT_LESSISI) _527_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T10_HTHM (in landau2)
 (conclusion conc (IMPLIES _527_T9 _527_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T11_HTHM (in landau2)
 (conclusion conc (IMPLIES _527_T10 _527_T11))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T12_HTHM (in landau2)
 (conclusion conc (IMPLIES _ANDE2 _527_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _527_T13_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _527_T12 _527_T11) _ANDI) _527_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T35_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _FRIS _REFEQ1) _SATZ28A) _EQND) _TFEQ1A) _TR3EQ) _II5_T35))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _II5_T36_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _II5_T35 _INCLASS) _TICT) _ISI) _II5_T36))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _EXAMPLE1A_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _II5_T36 _RATAPP1) _EXAMPLE1A))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5114_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _SATZ40C _COMTS) _EQN) _TFEQ2A) _TR3EQ) _5114_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ114_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _5114_T1 _INCLASS) _TICT) _ISI) _SATZ114))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ110C_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ110 _SATZ110C))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ110G_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _SATZ110C _SATZ110B) _SATZ110G))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _SATZ114B_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _SATZ110C _SATZ114A) _SATZ110B) _SATZ114B))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T1_HTHM (in landau2)
 (conclusion conc (IMPLIES _SATZ89 _5115_T1))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T3_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND (AND _SATZ111E _SATZ111D) _SATZ24) _TH9) _INCLASS) _MOREISI) _5115_T3))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T6_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5115_T5 _RT_MOREISI1) _5115_T6))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T8_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND _5115_T7 _5115_T6) _5115_T3) _ORAPP) _5115_T8))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T9_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND (AND (AND _5115_T2 _SATZ105D) _SATZ110C) _EXAMPLE1B) _RT_ISMORE12) _5115_T9))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T10_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _5115_T9 _5115_T8) _SATZ87C) _5115_T10))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T12_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5115_T11 _RATAPP1) _5115_T12))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T13_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND (AND _5115_T10 _NATRTI) _ANDI) _5115_T13))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

(th~defproblem _5115_T15_HTHM (in landau2)
 (conclusion conc (IMPLIES (AND _5115_T14 _RATAPP1) _5115_T15))
 (help "Theorem from Jutting's translation of Landau's Grundlagen
relativized using hypotheses.  The hypotheses were determined (automatically)
by analyzing the Automath proof term to determine which previously given
theorems and axioms were used in the proof."))

