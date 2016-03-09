; Chad E Brown; In November 2005 I translated Jutting's Automath version of
; Landau's Grundlagen der Analysis into TPS (without proofs) and then
; from TPS into this post syntax for Omega.  Enjoy!

(th~deftheory landau2
              (uses landau1)
	      (help "Definitions, constants, axioms and problems generated from
Jutting's Automath translation of Landau's analysis book.

Landau Chapter 2"))

(th~defdef _FRAC (in landau2)
 (definition (_PAIR1TYPE =))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FRAC_DEC (in landau2)
 (definition (PER _FRAC))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FR (in landau2)
 (definition (_PAIR1 =))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FR_DEC (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x2 x3) (_FRAC (_FR x0 x2) (_FR x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NUM (in landau2)
 (definition (_FIRST1 =))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NUM_DEC (in landau2)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DEN (in landau2)
 (definition (_SECOND1 =))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DEN_DEC (in landau2)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NUMIS (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_NUM (_FR x0 x1)) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNUM (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= x0 (_NUM (_FR x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DENIS (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_DEN (_FR x0 x1)) x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISDEN (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= x1 (_DEN (_FR x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FRIS (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_FRAC (_FR (_NUM x0) (_DEN x0)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISFR (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_FRAC x0 (_FR (_NUM x0) (_DEN x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _12ISND (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES x0 x3) (_TIMES (_NUM (_FR x0 x1)) (_DEN (_FR x2 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NDIS12 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES (_NUM (_FR x0 x1)) (_DEN (_FR x2 x3))) (_TIMES x0 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1DISND (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES x1 (_DEN x0)) (_TIMES (_NUM (_FR x1 x2)) (_DEN x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NDIS1D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_NUM (_FR x1 x2)) (_DEN x0)) (_TIMES x1 (_DEN x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _N2ISND (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_NUM x0) x2) (_TIMES (_NUM x0) (_DEN (_FR x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NDISN2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_NUM x0) (_DEN (_FR x1 x2))) (_TIMES (_NUM x0) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISN (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x2) (_FRAC (_FR x0 x1) (_FR x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISD (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x1 x2) (_FRAC (_FR x0 x1) (_FR x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISND (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x2) (IMPLIES (= x1 x3) (_FRAC (_FR x0 x1) (_FR x2 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQ (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (= (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQI12 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= (_TIMES x0 x3) (_TIMES x2 x1)) (_EQ (_FR x0 x1) (_FR x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQI1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_TIMES (_NUM x0) x2) (_TIMES x1 (_DEN x0))) (_EQ x0 (_FR x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQI2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_TIMES x1 (_DEN x0)) (_TIMES (_NUM x0) x2)) (_EQ (_FR x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ37 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_EQ x0 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFEQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_EQ x0 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFEQ1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_FRAC x0 x1) (_EQ x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFEQ2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_FRAC x0 x1) (_EQ x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQND (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x2) (IMPLIES (= x1 x3) (_EQ (_FR x0 x1) (_FR x2 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQN (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x2) (_EQ (_FR x0 x1) (_FR x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQD (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x1 x2) (_EQ (_FR x0 x1) (_FR x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ38 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_EQ x0 x1) (_EQ x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SYMEQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_EQ x0 x1) (_EQ x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II1_T1 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES x1 (_TIMES x2 x3)) (_TIMES x3 (_TIMES x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _STETS (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES (_TIMES x0 x1) (_TIMES x2 x3)) (_TIMES (_TIMES x0 x3) (_TIMES x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II1_T2 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES x1 (_TIMES x2 x3)) (_TIMES x3 (_TIMES x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II1_ANDERS (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES (_TIMES x0 x1) (_TIMES x2 x3)) (_TIMES (_TIMES x0 x3) (_TIMES x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _139_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (= (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x2) (_DEN x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _139_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (= (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x1) (_DEN x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _139_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (= (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x2) (_DEN x1))) (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_TIMES (_NUM x1) (_DEN x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _139_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (= (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x1) (_DEN x1))) (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_TIMES (_NUM x1) (_DEN x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ39 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (_EQ x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _139_ANDERS (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (= (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x1) (_DEN x1))) (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_TIMES (_NUM x1) (_DEN x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TREQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (_EQ x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TREQ1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x2 x0) (IMPLIES (_EQ x2 x1) (_EQ x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TREQ2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x2) (_EQ x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TR3EQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (IMPLIES (_EQ x2 x3) (_EQ x0 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TR4EQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (IMPLIES (_EQ x2 x3) (IMPLIES (_EQ x3 x4) (_EQ x0 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ40 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (_EQ x0 (_FR (_TIMES (_NUM x0) x1) (_TIMES (_DEN x0) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ40A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (_EQ (_FR (_TIMES (_NUM x0) x1) (_TIMES (_DEN x0) x1)) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ40B (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_FR x0 x1) (_FR (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ40C (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_FR (_TIMES x0 x2) (_TIMES x1 x2)) (_FR x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREF (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_MORE (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSF (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_LESS (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREFI12 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE (_TIMES x0 x3) (_TIMES x2 x1)) (_MOREF (_FR x0 x1) (_FR x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSFI12 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESS (_TIMES x0 x3) (_TIMES x2 x1)) (_LESSF (_FR x0 x1) (_FR x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREFI1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE (_TIMES (_NUM x0) x2) (_TIMES x1 (_DEN x0))) (_MOREF x0 (_FR x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREFI2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE (_TIMES x1 (_DEN x0)) (_TIMES (_NUM x0) x2)) (_MOREF (_FR x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSFI1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS (_TIMES (_NUM x0) x2) (_TIMES x1 (_DEN x0))) (_LESSF x0 (_FR x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSFI2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS (_TIMES x1 (_DEN x0)) (_TIMES (_NUM x0) x2)) (_LESSF (_FR x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (AND (OR (_EQ x0 x1) (OR (_MOREF x0 x1) (_LESSF x0 x1))) (AND (IMPLIES (_EQ x0 x1) (NOT (_MOREF x0 x1))) (AND (IMPLIES (_MOREF x0 x1) (NOT (_LESSF x0 x1))) (IMPLIES (_LESSF x0 x1) (NOT (_EQ x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (OR (_EQ x0 x1) (OR (_MOREF x0 x1) (_LESSF x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (AND (IMPLIES (_EQ x0 x1) (NOT (_MOREF x0 x1))) (AND (IMPLIES (_MOREF x0 x1) (NOT (_LESSF x0 x1))) (IMPLIES (_LESSF x0 x1) (NOT (_EQ x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ42 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (_LESSF x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ43 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_MOREF x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _244_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (= (_TIMES (_TIMES (_NUM x1) (_DEN x3)) (_TIMES (_NUM x2) (_DEN x0))) (_TIMES (_TIMES (_NUM x3) (_DEN x1)) (_TIMES (_NUM x0) (_DEN x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _244_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (= (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x2) (_DEN x3))) (_TIMES (_TIMES (_NUM x3) (_DEN x2)) (_TIMES (_NUM x0) (_DEN x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _244_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (_MORE (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x2) (_DEN x3))) (_TIMES (_TIMES (_NUM x3) (_DEN x2)) (_TIMES (_NUM x1) (_DEN x0))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ44 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (_MOREF x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQMOREF12 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (IMPLIES (_MOREF x0 x2) (_MOREF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQMOREF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x0 x2) (_MOREF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQMOREF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x2 x0) (_MOREF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ45 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSF x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (_LESSF x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQLESSF12 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (IMPLIES (_LESSF x0 x2) (_LESSF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQLESSF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSF x0 x2) (_LESSF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQLESSF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSF x2 x0) (_LESSF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREQ (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (OR (_MOREF x0 x1) (_EQ x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSEQ (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (OR (_LESSF x0 x1) (_EQ x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREQI2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_EQ x0 x1) (_MOREQ x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSEQI2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_EQ x0 x1) (_LESSEQ x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREQI1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (_MOREQ x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSEQI1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_LESSEQ x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREQ x0 x1) (NOT (_LESSF x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSEQ x0 x1) (NOT (_MOREF x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41E (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (NOT (_MOREF x0 x1)) (_LESSEQ x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41F (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (NOT (_LESSF x0 x1)) (_MOREQ x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41G (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (NOT (_LESSEQ x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41H (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (NOT (_MOREQ x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41J (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (NOT (_MOREQ x0 x1)) (_LESSF x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ41K (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (NOT (_LESSEQ x0 x1)) (_MOREF x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _246_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (IMPLIES (_MOREF x0 x1) (_MOREQ x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _246_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (IMPLIES (_EQ x0 x1) (_MOREQ x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ46 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (_MOREQ x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQMOREQ12 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (IMPLIES (_MOREQ x0 x2) (_MOREQ x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQMOREQ1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREQ x0 x2) (_MOREQ x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQMOREQ2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREQ x2 x0) (_MOREQ x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _247_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (IMPLIES (_LESSF x0 x1) (_LESSEQ x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _247_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (IMPLIES (_EQ x0 x1) (_LESSEQ x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ47 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_EQ x0 x2) (IMPLIES (_EQ x1 x3) (_LESSEQ x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQLESSEQ12 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (IMPLIES (_LESSEQ x0 x2) (_LESSEQ x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQLESSEQ1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSEQ x0 x2) (_LESSEQ x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQLESSEQ2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSEQ x2 x0) (_LESSEQ x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ48 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREQ x0 x1) (_LESSEQ x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ49 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSEQ x0 x1) (_MOREQ x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _250_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSF x1 x2) (_LESS (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x2) (_DEN x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _250_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSF x1 x2) (_LESS (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x1) (_DEN x1))) (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_TIMES (_NUM x1) (_DEN x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ50 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSF x1 x2) (_LESSF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRLESSF (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSF x1 x2) (_LESSF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRMOREF (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREF x1 x2) (_MOREF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ51A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSF x1 x2) (_LESSF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ51B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSEQ x1 x2) (_LESSF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ51C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREF x1 x2) (_MOREF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ51D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREQ x1 x2) (_MOREF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _252_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x1 x2) (_LESSEQ x0 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _252_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSF x1 x2) (_LESSEQ x0 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _252_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (IMPLIES (_EQ x0 x1) (_LESSEQ x0 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _252_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (IMPLIES (_LESSF x0 x1) (_LESSEQ x0 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ52 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (_LESSEQ x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRLESSEQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (_LESSEQ x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _252_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (IMPLIES (_LESSF x0 x1) (_LESSEQ x0 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _252_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (IMPLIES (_EQ x0 x1) (_LESSEQ x0 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _252_ANDERS (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x1 x2) (_LESSEQ x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRMOREQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x1 x2) (_MOREQ x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _253_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_MORE (_TIMES (_PL (_NUM x0) (_NUM x0)) (_DEN x0)) (_TIMES (_NUM x0) (_DEN x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _253_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_MOREF (_FR (_PL (_NUM x0) (_NUM x0)) (_DEN x0)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ53 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (EXISTS (LAM (x1 (I I)) (AND (_FRAC x1 x1) (_MOREF x1 x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _254_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_LESS (_TIMES (_NUM x0) (_DEN x0)) (_TIMES (_NUM x0) (_PL (_DEN x0) (_DEN x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _254_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_LESSF (_FR (_NUM x0) (_PL (_DEN x0) (_DEN x0))) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ54 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (EXISTS (LAM (x1 (I I)) (AND (_FRAC x1 x1) (_LESSF x1 x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _255_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_LESS (_PL (_TIMES (_NUM x0) (_DEN x0)) (_TIMES (_NUM x0) (_DEN x1))) (_PL (_TIMES (_NUM x0) (_DEN x0)) (_TIMES (_NUM x1) (_DEN x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _255_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_LESS (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x1))) (_PL (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x1) (_DEN x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _255_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_LESS (_TIMES (_NUM x0) (_PL (_DEN x0) (_DEN x1))) (_TIMES (_PL (_NUM x0) (_NUM x1)) (_DEN x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _255_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_LESSF x0 (_FR (_PL (_NUM x0) (_NUM x1)) (_PL (_DEN x0) (_DEN x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _255_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_LESS (_TIMES (_PL (_NUM x0) (_NUM x1)) (_DEN x1)) (_TIMES (_NUM x1) (_PL (_DEN x0) (_DEN x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _255_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (_LESSF (_FR (_PL (_NUM x0) (_NUM x1)) (_PL (_DEN x0) (_DEN x1))) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _255_T7 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (AND (_LESSF x0 (_FR (_PL (_NUM x0) (_NUM x1)) (_PL (_DEN x0) (_DEN x1)))) (_LESSF (_FR (_PL (_NUM x0) (_NUM x1)) (_PL (_DEN x0) (_DEN x1))) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ55 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSF x0 x1) (EXISTS (LAM (x2 (I I)) (AND (_FRAC x2 x2) (AND (_LESSF x0 x2) (_LESSF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PF (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_FR (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_TIMES (_DEN x0) (_DEN x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PF_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (_FRAC (_PF x0 x2) (_PF x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II3_T1 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_PL (_TIMES (_NUM (_FR x0 x1)) (_DEN (_FR x2 x3))) (_TIMES (_NUM (_FR x2 x3)) (_DEN (_FR x0 x1)))) (_PL (_TIMES x0 x3) (_TIMES x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II3_T2 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES (_DEN (_FR x0 x1)) (_DEN (_FR x2 x3))) (_TIMES x1 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PF12 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (_FRAC (_PF (_FR x0 x1) (_FR x2 x3)) (_FR (_PL (_TIMES x0 x3) (_TIMES x2 x1)) (_TIMES x1 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II3_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_PL (_TIMES (_NUM x0) (_DEN (_FR x1 x2))) (_TIMES (_NUM (_FR x1 x2)) (_DEN x0))) (_PL (_TIMES (_NUM x0) x2) (_TIMES x1 (_DEN x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II3_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_DEN x0) (_DEN (_FR x1 x2))) (_TIMES (_DEN x0) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_FRAC (_PF x0 (_FR x1 x2)) (_FR (_PL (_TIMES (_NUM x0) x2) (_TIMES x1 (_DEN x0))) (_TIMES (_DEN x0) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II3_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_PL (_TIMES (_NUM (_FR x1 x2)) (_DEN x0)) (_TIMES (_NUM x0) (_DEN (_FR x1 x2)))) (_PL (_TIMES x1 (_DEN x0)) (_TIMES (_NUM x0) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II3_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_DEN (_FR x1 x2)) (_DEN x0)) (_TIMES x2 (_DEN x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_FRAC (_PF (_FR x1 x2) x0) (_FR (_PL (_TIMES x1 (_DEN x0)) (_TIMES (_NUM x0) x2)) (_TIMES x2 (_DEN x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PFEQ12A (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (_EQ (_PF (_FR x0 x1) (_FR x2 x3)) (_FR (_PL (_TIMES x0 x3) (_TIMES x2 x1)) (_TIMES x1 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PFEQ12B (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (_EQ (_FR (_PL (_TIMES x0 x3) (_TIMES x2 x1)) (_TIMES x1 x3)) (_PF (_FR x0 x1) (_FR x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PFEQ1A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_PF x0 (_FR x1 x2)) (_FR (_PL (_TIMES (_NUM x0) x2) (_TIMES x1 (_DEN x0))) (_TIMES (_DEN x0) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PFEQ1B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_FR (_PL (_TIMES (_NUM x0) x2) (_TIMES x1 (_DEN x0))) (_TIMES (_DEN x0) x2)) (_PF x0 (_FR x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PFEQ2A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_PF (_FR x1 x2) x0) (_FR (_PL (_TIMES x1 (_DEN x0)) (_TIMES (_NUM x0) x2)) (_TIMES x2 (_DEN x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PFEQ2B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_FR (_PL (_TIMES x1 (_DEN x0)) (_TIMES (_NUM x0) x2)) (_TIMES x2 (_DEN x0))) (_PF (_FR x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _356_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_DEN x2) (_DEN x3))) (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_DEN x2) (_DEN x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _356_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_TIMES (_NUM x2) (_DEN x3)) (_TIMES (_DEN x0) (_DEN x1))) (_TIMES (_TIMES (_NUM x3) (_DEN x2)) (_TIMES (_DEN x0) (_DEN x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _356_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_DEN x1) (_DEN x3))) (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_DEN x2) (_DEN x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _356_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_DEN x1) (_DEN x3))) (_TIMES (_TIMES (_NUM x1) (_DEN x3)) (_TIMES (_DEN x0) (_DEN x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _356_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_TIMES (_DEN x1) (_DEN x3))) (_TIMES (_TIMES (_NUM x3) (_DEN x1)) (_TIMES (_DEN x0) (_DEN x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _356_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_PL (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_DEN x1) (_DEN x3))) (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_TIMES (_DEN x1) (_DEN x3)))) (_PL (_TIMES (_TIMES (_NUM x1) (_DEN x3)) (_TIMES (_DEN x0) (_DEN x2))) (_TIMES (_TIMES (_NUM x3) (_DEN x1)) (_TIMES (_DEN x0) (_DEN x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _356_T7 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x0))) (_TIMES (_DEN x1) (_DEN x3))) (_TIMES (_PL (_TIMES (_NUM x1) (_DEN x3)) (_TIMES (_NUM x3) (_DEN x1))) (_TIMES (_DEN x0) (_DEN x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ56 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_EQ (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQPF12 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_EQ (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQPF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_PF x0 x2) (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQPF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_PF x2 x0) (_PF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ57 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_PF (_FR x0 x2) (_FR x1 x2)) (_FR (_PL x0 x1) x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ57A (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_FR (_PL x0 x1) x2) (_PF (_FR x0 x2) (_FR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ58 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_EQ (_PF x0 x1) (_PF x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMPF (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_EQ (_PF x0 x1) (_PF x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_DEN x2)) (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_PL (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_DEN x2)) (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_DEN x2))) (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_DEN x2)) (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_TIMES (_NUM x2) (_TIMES (_DEN x0) (_DEN x1))) (_TIMES (_TIMES (_NUM x2) (_DEN x1)) (_DEN x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_PL (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_DEN x2)) (_TIMES (_NUM x2) (_TIMES (_DEN x0) (_DEN x1)))) (_PL (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0))) (_TIMES (_TIMES (_NUM x2) (_DEN x1)) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_PL (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0)) (_TIMES (_TIMES (_NUM x2) (_DEN x1)) (_DEN x0)))) (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_PL (_TIMES (_NUM x1) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x1))) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T7 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_PL (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_DEN x2)) (_TIMES (_NUM x2) (_TIMES (_DEN x0) (_DEN x1)))) (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_PL (_TIMES (_NUM x1) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x1))) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ59 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_PF (_PF x0 x1) x2) (_PF x0 (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSPF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_PF (_PF x0 x1) x2) (_PF x0 (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSPF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_PF x0 (_PF x1 x2)) (_PF (_PF x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _STETS1 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_TIMES x0 x1) x2) (_TIMES (_TIMES x0 x2) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T8 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_DEN x2)) (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _359_T9 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_TIMES (_NUM x2) (_TIMES (_DEN x0) (_DEN x1))) (_TIMES (_TIMES (_NUM x2) (_DEN x1)) (_DEN x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDERST7 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (= (_PL (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_DEN x2)) (_TIMES (_NUM x2) (_TIMES (_DEN x0) (_DEN x1)))) (_PL (_TIMES (_NUM x0) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_PL (_TIMES (_NUM x1) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x1))) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _360_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_MORE (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_TIMES (_NUM x0) (_DEN x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _360_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_MORE (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_DEN x0)) (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_DEN x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _360_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (= (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_DEN x0)) (_TIMES (_NUM x0) (_TIMES (_DEN x0) (_DEN x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _360_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_MORE (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))) (_DEN x0)) (_TIMES (_NUM x0) (_TIMES (_DEN x0) (_DEN x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ60 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_MOREF (_PF x0 x1) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ60A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_LESSF x0 (_PF x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _361_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_DEN x2)) (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_DEN x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _361_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_DEN x1)) (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _361_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (= (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_DEN x1)) (_TIMES (_TIMES (_NUM x2) (_DEN x1)) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _361_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_PL (_TIMES (_TIMES (_NUM x0) (_DEN x2)) (_DEN x1)) (_TIMES (_TIMES (_NUM x2) (_DEN x0)) (_DEN x1))) (_PL (_TIMES (_TIMES (_NUM x1) (_DEN x2)) (_DEN x0)) (_TIMES (_TIMES (_NUM x2) (_DEN x1)) (_DEN x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _361_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x0))) (_DEN x1)) (_TIMES (_PL (_TIMES (_NUM x1) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x1))) (_DEN x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _361_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_TIMES (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x0))) (_DEN x1)) (_DEN x2)) (_TIMES (_TIMES (_PL (_TIMES (_NUM x1) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x1))) (_DEN x0)) (_DEN x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _361_T7 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_TIMES (_PL (_TIMES (_NUM x0) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x0))) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_PL (_TIMES (_NUM x1) (_DEN x2)) (_TIMES (_NUM x2) (_DEN x1))) (_TIMES (_DEN x0) (_DEN x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ61 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MOREF (_PF x0 x2) (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MOREF (_PF x0 x2) (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_PF x0 x2) (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (_LESSF (_PF x0 x2) (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MOREF (_PF x2 x0) (_PF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62E (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_PF x2 x0) (_PF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62F (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (_LESSF (_PF x2 x0) (_PF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62G (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62H (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_PF x2 x0) (_PF x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62J (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ62K (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_PF x2 x0) (_PF x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _363_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (OR (_EQ x0 x1) (OR (_MOREF x0 x1) (_LESSF x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _363_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (AND (IMPLIES (_EQ (_PF x0 x2) (_PF x1 x2)) (NOT (_MOREF (_PF x0 x2) (_PF x1 x2)))) (AND (IMPLIES (_MOREF (_PF x0 x2) (_PF x1 x2)) (NOT (_LESSF (_PF x0 x2) (_PF x1 x2)))) (IMPLIES (_LESSF (_PF x0 x2) (_PF x1 x2)) (NOT (_EQ (_PF x0 x2) (_PF x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ63A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF (_PF x0 x2) (_PF x1 x2)) (_MOREF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ63B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ (_PF x0 x2) (_PF x1 x2)) (_EQ x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ63C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF (_PF x0 x2) (_PF x1 x2)) (_LESSF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ63D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF (_PF x2 x0) (_PF x2 x1)) (_MOREF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ63E (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ (_PF x2 x0) (_PF x2 x1)) (_EQ x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ63F (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF (_PF x2 x0) (_PF x2 x1)) (_LESSF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _364_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_PF x0 x2) (_PF x1 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _364_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_PF x1 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ64 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ64A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ65A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ65B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREQ x2 x3) (_MOREF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ65C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ65D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSEQ x2 x3) (_LESSF (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _366_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_MOREQ (_PF x0 x2) (_PF x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _366_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREQ (_PF x0 x2) (_PF x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _366_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_EQ x0 x1) (_MOREQ (_PF x0 x2) (_PF x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _366_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_MOREF x0 x1) (_MOREQ (_PF x0 x2) (_PF x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ66 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (_MOREQ (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ66A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x2 x3) (_LESSEQ (_PF x0 x2) (_PF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _367_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSEQ x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ (_PF x1 x2) x0) (_MOREF x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _367_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSEQ x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (NOT (_EQ (_PF x1 x2) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _VORBEMERKUNG67 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_LESSEQ x0 x1) (NOT (EXISTS (LAM (x2 (I I)) (AND (_FRAC x2 x2) (_EQ (_PF x1 x2) x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ67B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ (_PF x1 x2) x0) (IMPLIES (_EQ (_PF x1 x3) x0) (_EQ x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _367_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (_N_ONE (_DIFFPROP (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _VO (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (THAT (_DIFFPROP (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x1) (_DEN x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _VO_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (IMPLIES (_MOREF x0 x2) (= (_VO x0 x2) (_VO x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _367_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (= (_TIMES (_NUM x0) (_DEN x1)) (_PL (_TIMES (_NUM x1) (_DEN x0)) (_VO x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _W (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_FR (_VO x0 x1) (_TIMES (_DEN x0) (_DEN x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _W_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (IMPLIES (_MOREF x0 x2) (_FRAC (_W x0 x2) (_W x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _367_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (_EQ x1 (_FR (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_DEN x0) (_DEN x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _367_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (_EQ (_PF x1 (_W x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ67A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (EXISTS (LAM (x2 (I I)) (AND (_FRAC x2 x2) (_EQ (_PF x1 x2) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MF (in landau2)
 (definition _W)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MF_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (IMPLIES (_MOREF x0 x2) (_FRAC (_MF x0 x2) (_MF x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ67C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (_EQ (_PF x1 (_MF x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ67D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_MOREF x0 x1) (_EQ x0 (_PF x1 (_MF x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ67E (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (IMPLIES (_EQ (_PF x1 x2) x0) (_EQ x2 (_MF x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TF (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_FR (_TIMES (_NUM x0) (_NUM x1)) (_TIMES (_DEN x0) (_DEN x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TF_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (_FRAC (_TF x0 x2) (_TF x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II4_T1 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES (_NUM (_FR x0 x1)) (_NUM (_FR x2 x3))) (_TIMES x0 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II4_T2 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES (_DEN (_FR x0 x1)) (_DEN (_FR x2 x3))) (_TIMES x1 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TF12 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (_FRAC (_TF (_FR x0 x1) (_FR x2 x3)) (_FR (_TIMES x0 x2) (_TIMES x1 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II4_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_NUM x0) (_NUM (_FR x1 x2))) (_TIMES (_NUM x0) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II4_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_DEN x0) (_DEN (_FR x1 x2))) (_TIMES (_DEN x0) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_FRAC (_TF x0 (_FR x1 x2)) (_FR (_TIMES (_NUM x0) x1) (_TIMES (_DEN x0) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II4_T5 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_NUM (_FR x1 x2)) (_NUM x0)) (_TIMES x1 (_NUM x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II4_T6 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_DEN (_FR x1 x2)) (_DEN x0)) (_TIMES x2 (_DEN x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_FRAC (_TF (_FR x1 x2) x0) (_FR (_TIMES x1 (_NUM x0)) (_TIMES x2 (_DEN x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TFEQ12A (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (_EQ (_TF (_FR x0 x1) (_FR x2 x3)) (_FR (_TIMES x0 x2) (_TIMES x1 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TFEQ12B (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (_EQ (_FR (_TIMES x0 x2) (_TIMES x1 x3)) (_TF (_FR x0 x1) (_FR x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TFEQ1A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_TF x0 (_FR x1 x2)) (_FR (_TIMES (_NUM x0) x1) (_TIMES (_DEN x0) x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TFEQ1B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_FR (_TIMES (_NUM x0) x1) (_TIMES (_DEN x0) x2)) (_TF x0 (_FR x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TFEQ2A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_TF (_FR x1 x2) x0) (_FR (_TIMES x1 (_NUM x0)) (_TIMES x2 (_DEN x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TFEQ2B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (_EQ (_FR (_TIMES x1 (_NUM x0)) (_TIMES x2 (_DEN x0))) (_TF (_FR x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _468_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x2) (_DEN x3))) (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x3) (_DEN x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _STETS2 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (= (_TIMES (_TIMES x0 x1) (_TIMES x2 x3)) (_TIMES (_TIMES x0 x2) (_TIMES x1 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _468_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (= (_TIMES (_TIMES (_NUM x0) (_NUM x2)) (_TIMES (_DEN x1) (_DEN x3))) (_TIMES (_TIMES (_NUM x1) (_NUM x3)) (_TIMES (_DEN x0) (_DEN x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ68 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_EQ (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQTF12 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_EQ (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQTF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_TF x0 x2) (_TF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EQTF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_TF x2 x0) (_TF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ69 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_EQ (_TF x0 x1) (_TF x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMTF (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_EQ (_TF x0 x1) (_TF x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ70 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_TF (_TF x0 x1) x2) (_TF x0 (_TF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSTF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_TF (_TF x0 x1) x2) (_TF x0 (_TF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSTF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_TF x0 (_TF x1 x2)) (_TF (_TF x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _471_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_TF x0 (_PF x1 x2)) (_PF (_FR (_TIMES (_NUM x0) (_TIMES (_NUM x1) (_DEN x2))) (_TIMES (_DEN x0) (_TIMES (_DEN x1) (_DEN x2)))) (_FR (_TIMES (_NUM x0) (_TIMES (_NUM x2) (_DEN x1))) (_TIMES (_DEN x0) (_TIMES (_DEN x1) (_DEN x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _471_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_FR (_TIMES (_NUM x0) (_TIMES (_NUM x1) (_DEN x2))) (_TIMES (_DEN x0) (_TIMES (_DEN x1) (_DEN x2)))) (_TF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _471_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_FR (_TIMES (_NUM x0) (_TIMES (_NUM x2) (_DEN x1))) (_TIMES (_DEN x0) (_TIMES (_DEN x1) (_DEN x2)))) (_TF x0 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ71 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_TF x0 (_PF x1 x2)) (_PF (_TF x0 x1) (_TF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTTPF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_TF (_PF x0 x1) x2) (_PF (_TF x0 x2) (_TF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTTPF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_TF x0 (_PF x1 x2)) (_PF (_TF x0 x1) (_TF x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTPTF1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_PF (_TF x0 x2) (_TF x1 x2)) (_TF (_PF x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTPTF2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (_EQ (_PF (_TF x0 x1) (_TF x0 x2)) (_TF x0 (_PF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _472_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_TIMES (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_NUM x2) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_DEN x0)) (_TIMES (_NUM x2) (_DEN x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _472_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MORE (_TIMES (_TIMES (_NUM x0) (_NUM x2)) (_TIMES (_DEN x1) (_DEN x2))) (_TIMES (_TIMES (_NUM x1) (_NUM x2)) (_TIMES (_DEN x0) (_DEN x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MOREF (_TF x0 x2) (_TF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_TF x0 x2) (_TF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (_LESSF (_TF x0 x2) (_TF x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x0 x1) (_MOREF (_TF x2 x0) (_TF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72E (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ x0 x1) (_EQ (_TF x2 x0) (_TF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72F (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x0 x1) (_LESSF (_TF x2 x0) (_TF x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72G (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72H (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_TF x2 x0) (_TF x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72J (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ72K (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_TF x2 x0) (_TF x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _473_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (OR (_EQ x0 x1) (OR (_MOREF x0 x1) (_LESSF x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _473_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (AND (IMPLIES (_EQ (_TF x0 x2) (_TF x1 x2)) (NOT (_MOREF (_TF x0 x2) (_TF x1 x2)))) (AND (IMPLIES (_MOREF (_TF x0 x2) (_TF x1 x2)) (NOT (_LESSF (_TF x0 x2) (_TF x1 x2)))) (IMPLIES (_LESSF (_TF x0 x2) (_TF x1 x2)) (NOT (_EQ (_TF x0 x2) (_TF x1 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ73A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF (_TF x0 x2) (_TF x1 x2)) (_MOREF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ73B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ (_TF x0 x2) (_TF x1 x2)) (_EQ x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ73C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF (_TF x0 x2) (_TF x1 x2)) (_LESSF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ73D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF (_TF x2 x0) (_TF x2 x1)) (_MOREF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ73E (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_EQ (_TF x2 x0) (_TF x2 x1)) (_EQ x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ73F (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF (_TF x2 x0) (_TF x2 x1)) (_LESSF x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _474_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_TF x0 x2) (_TF x1 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _474_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_TF x1 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ74 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ74A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ75A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ75B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREF x0 x1) (IMPLIES (_MOREQ x2 x3) (_MOREF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ75C (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSF x2 x3) (_LESSF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ75D (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSF x0 x1) (IMPLIES (_LESSEQ x2 x3) (_LESSF (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _476_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_MOREQ (_TF x0 x2) (_TF x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _476_T2 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_MOREF x2 x3) (_MOREQ (_TF x0 x2) (_TF x1 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _476_T3 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_EQ x0 x1) (_MOREQ (_TF x0 x2) (_TF x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _476_T4 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (IMPLIES (_MOREF x0 x1) (_MOREQ (_TF x0 x2) (_TF x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ76 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_MOREQ x0 x1) (IMPLIES (_MOREQ x2 x3) (_MOREQ (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ76A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_LESSEQ x0 x1) (IMPLIES (_LESSEQ x2 x3) (_LESSEQ (_TF x0 x2) (_TF x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ77B (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ (_TF x1 x2) x0) (IMPLIES (_EQ (_TF x1 x3) x0) (_EQ x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _V (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_FR (_TIMES (_NUM x0) (_DEN x1)) (_TIMES (_DEN x0) (_NUM x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _V_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (_FRAC (_V x0 x2) (_V x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _477_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (_EQ (_TF x1 (_V x0 x1)) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ77A (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (EXISTS (LAM (x2 (I I)) (AND (_FRAC x2 x2) (_EQ (_TF x1 x2) x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_EQ (in landau2)
 (definition _EQ)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_REFEQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_RT_EQ x0 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_SYMEQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_RT_EQ x0 x1) (_RT_EQ x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_TREQ (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_RT_EQ x0 x1) (IMPLIES (_RT_EQ x1 x2) (_RT_EQ x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INF (in landau2)
 (definition (_ESTI _FRAC))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RAT (in landau2)
 (definition (_ECT _FRAC _RT_EQ))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RAT_DEC (in landau2)
 (definition (PER _RAT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_IS (in landau2)
 (definition _RAT)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_NIS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (NOT (_RT_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_SOME (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (EXISTS (LAM (x1 (O (I I))) (AND (_RAT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ALL (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ONE (in landau2)
 (definition (_E_ONE _RAT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_IN (in landau2)
 (definition (_ESTI _RAT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATOF (in landau2)
 (definition (_ECTELT _FRAC _RT_EQ))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATOF_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (_RAT (_RATOF x0) (_RATOF x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLASS (in landau2)
 (definition (_ECECT _FRAC _RT_EQ))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CLASS_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (_SET _FRAC (_CLASS x0) (_CLASS x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INCLASS (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_INF x0 (_CLASS (_RATOF x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEMMAEQ1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_INF x1 (_CLASS x0)) (IMPLIES (_EQ x1 x2) (_INF x2 (_CLASS x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATAPP1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 O) (IMPLIES (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_INF x2 (_CLASS x0)) x1)))) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) x2)))))))) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x5 (_CLASS x0)) x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATAPP2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) x2)))))))) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) x3)))))))))))) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x7 (_CLASS x0)) x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATAPP3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) x3)))))))))))) x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (FORALL (LAM (x4 O) (IMPLIES (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (FORALL (LAM (x8 (I I)) (IMPLIES (_FRAC x8 x8) (IMPLIES (_INF x5 (_CLASS x0)) (IMPLIES (_INF x6 (_CLASS x1)) (IMPLIES (_INF x7 (_CLASS x2)) (IMPLIES (_INF x8 (_CLASS x3)) x4)))))))))))))))) (FORALL (LAM (x9 (I I)) (IMPLIES (_FRAC x9 x9) (IMPLIES (_INF x9 (_CLASS x0)) x4))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RATAPP4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (FORALL (LAM (x4 O) (IMPLIES (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (FORALL (LAM (x8 (I I)) (IMPLIES (_FRAC x8 x8) (IMPLIES (_INF x5 (_CLASS x0)) (IMPLIES (_INF x6 (_CLASS x1)) (IMPLIES (_INF x7 (_CLASS x2)) (IMPLIES (_INF x8 (_CLASS x3)) x4)))))))))))))))) x4))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_EQ x2 x3) (_RT_IS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_IS x0 x1) (_EQ x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NISI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (NOT (_EQ x2 x3)) (_RT_NIS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NISE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_NIS x0 x1) (NOT (_EQ x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIXF (in landau2)
 (TYPE-VARIABLES Z)
 (definition (_FIXFU2 _FRAC _RT_EQ))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INDRAT (in landau2)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (LAM (x2 (O Z Z)) (LAM (x3 (Z (I I) (I I))) (_INDEQ _FRAC _RT_EQ x2 (_I _FRAC _RT_EQ x2 x3 x0) x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INDRAT_DEC (in landau2)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O Z Z)) (IMPLIES (PER x4) (FORALL (LAM (x5 (Z (I I) (I I))) (FORALL (LAM (x6 (Z (I I) (I I))) (IMPLIES (FORALL (LAM (x7 (I I)) (FORALL (LAM (x8 (I I)) (IMPLIES (_FRAC x7 x8) (FORALL (LAM (x9 (I I)) (FORALL (LAM (x10 (I I)) (IMPLIES (_FRAC x9 x10) (x4 (x5 x7 x9) (x6 x8 x10)))))))))))) (IMPLIES (_FIXF x4 x5) (x4 (_INDRAT x0 x2 x4 x5) (_INDRAT x1 x3 x4 x6)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINDRAT (in landau2)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O Z Z)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Z (I I) (I I))) (IMPLIES (FORALL (LAM (x4 (I I)) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x4 x5) (FORALL (LAM (x6 (I I)) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x6 x7) (x2 (x3 x4 x6) (x3 x5 x7)))))))))))) (IMPLIES (_FIXF x2 x3) (FORALL (LAM (x8 (I I)) (IMPLIES (_FRAC x8 x8) (FORALL (LAM (x9 (I I)) (IMPLIES (_FRAC x9 x9) (IMPLIES (_INF x8 (_CLASS x0)) (IMPLIES (_INF x9 (_CLASS x1)) (x2 (x3 x8 x9) (_INDRAT x0 x1 x2 x3)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ78 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS x0 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ79 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IS x0 x1) (_RT_IS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ80 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_IS x1 x2) (_RT_IS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_MORE (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (EXISTS (LAM (x2 (I I)) (AND (_FRAC x2 x2) (EXISTS (LAM (x3 (I I)) (AND (_FRAC x3 x3) (AND (_INF x2 (_CLASS x0)) (AND (_INF x3 (_CLASS x1)) (_MOREF x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROPM (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (LAM (x2 (I I)) (LAM (x3 (I I)) (AND (_INF x2 (_CLASS x0)) (AND (_INF x3 (_CLASS x1)) (_MOREF x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPM x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPM x0 x1 x4 x6) (_INF x4 (_CLASS x0))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPM x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPM x0 x1 x4 x6) (_INF x6 (_CLASS x1))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPM x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPM x0 x1 x4 x6) (_MOREF x4 x6)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T7 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPM x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPM x0 x1 x4 x6) (_MOREF x2 x3)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T8 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPM x0 x1 x4 x5)))) (_MOREF x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ALSO18 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_MOREF x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T9 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_MOREF x2 x3) (_PROPM x0 x1 x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T10 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_MOREF x2 x3) (EXISTS (LAM (x4 (I I)) (AND (_FRAC x4 x4) (_PROPM x0 x1 x2 x4))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_MOREF x2 x3) (_RT_MORE x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_MORE x0 x1) (_MOREF x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISMORE1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_MORE x0 x2) (_RT_MORE x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISMORE2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_MORE x2 x0) (_RT_MORE x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISMORE12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_IS x2 x3) (IMPLIES (_RT_MORE x0 x2) (_RT_MORE x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_LESS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (EXISTS (LAM (x2 (I I)) (AND (_FRAC x2 x2) (EXISTS (LAM (x3 (I I)) (AND (_FRAC x3 x3) (AND (_INF x2 (_CLASS x0)) (AND (_INF x3 (_CLASS x1)) (_LESSF x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROPL (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (LAM (x2 (I I)) (LAM (x3 (I I)) (AND (_INF x2 (_CLASS x0)) (AND (_INF x3 (_CLASS x1)) (_LESSF x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T11 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPL x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPL x0 x1 x4 x6) (_INF x4 (_CLASS x0))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPL x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPL x0 x1 x4 x6) (_INF x6 (_CLASS x1))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T13 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPL x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPL x0 x1 x4 x6) (_LESSF x4 x6)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T14 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPL x0 x1 x4 x5)))) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (IMPLIES (_PROPL x0 x1 x4 x6) (_LESSF x2 x3)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T15 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (EXISTS (LAM (x5 (I I)) (AND (_FRAC x5 x5) (_PROPL x0 x1 x4 x5)))) (_LESSF x2 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ALSO19 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_LESSF x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T16 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_LESSF x2 x3) (_PROPL x0 x1 x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T17 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_LESSF x2 x3) (EXISTS (LAM (x4 (I I)) (AND (_FRAC x4 x4) (_PROPL x0 x1 x2 x4))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_LESSF x2 x3) (_RT_LESS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_LESS x0 x1) (_LESSF x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISLESS1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_LESS x0 x2) (_RT_LESS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISLESS2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_LESS x2 x0) (_RT_LESS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISLESS12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_IS x2 x3) (IMPLIES (_RT_LESS x0 x2) (_RT_LESS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (OR (_EQ x2 x3) (OR (_MOREF x2 x3) (_LESSF x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_EQ x2 x3) (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_MOREF x2 x3) (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_LESSF x2 x3) (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (AND (IMPLIES (_EQ x2 x3) (NOT (_MOREF x2 x3))) (AND (IMPLIES (_MOREF x2 x3) (NOT (_LESSF x2 x3))) (IMPLIES (_LESSF x2 x3) (NOT (_EQ x2 x3))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T7 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_IS x0 x1) (NOT (_RT_MORE x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T8 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_MORE x0 x1) (NOT (_RT_LESS x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T9 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_LESS x0 x1) (_RT_NIS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T10 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (AND (IMPLIES (_RT_IS x0 x1) (NOT (_RT_MORE x0 x1))) (AND (IMPLIES (_RT_MORE x0 x1) (NOT (_RT_LESS x0 x1))) (IMPLIES (_RT_LESS x0 x1) (NOT (_RT_IS x0 x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _581_T11 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (AND (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1))) (AND (IMPLIES (_RT_IS x0 x1) (NOT (_RT_MORE x0 x1))) (AND (IMPLIES (_RT_MORE x0 x1) (NOT (_RT_LESS x0 x1))) (IMPLIES (_RT_LESS x0 x1) (NOT (_RT_IS x0 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (AND (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1))) (AND (IMPLIES (_RT_IS x0 x1) (NOT (_RT_MORE x0 x1))) (AND (IMPLIES (_RT_MORE x0 x1) (NOT (_RT_LESS x0 x1))) (IMPLIES (_RT_LESS x0 x1) (NOT (_RT_IS x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (OR (_RT_IS x0 x1) (OR (_RT_MORE x0 x1) (_RT_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (AND (IMPLIES (_RT_IS x0 x1) (NOT (_RT_MORE x0 x1))) (AND (IMPLIES (_RT_MORE x0 x1) (NOT (_RT_LESS x0 x1))) (IMPLIES (_RT_LESS x0 x1) (NOT (_RT_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _582_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_LESS x1 x0)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ82 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_LESS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _583_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_MORE x1 x0)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ83 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (_RT_MORE x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_MOREIS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (OR (_RT_MORE x0 x1) (_RT_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_MOREISI1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_MOREIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_MOREISI2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IS x0 x1) (_RT_MOREIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREISI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_MOREQ x2 x3) (_RT_MOREIS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREISE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_MOREIS x0 x1) (_MOREQ x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISMOREIS1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_MOREIS x0 x2) (_RT_MOREIS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISMOREIS2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_MOREIS x2 x0) (_RT_MOREIS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISMOREIS12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_IS x2 x3) (IMPLIES (_RT_MOREIS x0 x2) (_RT_MOREIS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_LESSIS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (OR (_RT_LESS x0 x1) (_RT_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_LESSISI1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (_RT_LESSIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_LESSISI2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_IS x0 x1) (_RT_LESSIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSISI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_LESSEQ x2 x3) (_RT_LESSIS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSISE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (IMPLIES (_RT_LESSIS x0 x1) (_LESSEQ x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISLESSIS1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_LESSIS x0 x2) (_RT_LESSIS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISLESSIS2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_LESSIS x2 x0) (_RT_LESSIS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISLESSIS12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_IS x2 x3) (IMPLIES (_RT_LESSIS x0 x2) (_RT_LESSIS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MOREIS x0 x1) (NOT (_RT_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESSIS x0 x1) (NOT (_RT_MORE x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81E (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (NOT (_RT_MORE x0 x1)) (_RT_LESSIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81F (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (NOT (_RT_LESS x0 x1)) (_RT_MOREIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81G (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (NOT (_RT_LESSIS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81H (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (NOT (_RT_MOREIS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81J (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (NOT (_RT_MOREIS x0 x1)) (_RT_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ81K (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (NOT (_RT_LESSIS x0 x1)) (_RT_MORE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _584_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MOREIS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_LESSIS x1 x0)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ84 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MOREIS x0 x1) (_RT_LESSIS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _585_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESSIS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_MOREIS x1 x0)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ85 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESSIS x0 x1) (_RT_MOREIS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _586_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESS x1 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESS x0 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ86 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESS x1 x2) (_RT_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_TRLESS (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESS x1 x2) (_RT_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_TRMORE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MORE x1 x2) (_RT_MORE x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _587_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESS x1 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESS x0 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ87A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESS x1 x2) (_RT_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _587_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESSIS x1 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESS x0 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ87B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESSIS x1 x2) (_RT_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ87C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MORE x1 x2) (_RT_MORE x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ87D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MOREIS x1 x2) (_RT_MORE x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _588_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESSIS x1 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESSIS x0 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ88 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESSIS x1 x2) (_RT_LESSIS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_TRLESSIS (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESSIS x1 x2) (_RT_LESSIS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_TRMOREIS (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MOREIS x1 x2) (_RT_MOREIS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _589_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_MOREF x2 x1) (_RT_SOME (LAM (x3 (O (I I))) (_RT_MORE x3 x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _589_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_RT_SOME (LAM (x2 (O (I I))) (_RT_MORE x2 x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ89 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_SOME (LAM (x1 (O (I I))) (_RT_MORE x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _590_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (IMPLIES (_LESSF x2 x1) (_RT_SOME (LAM (x3 (O (I I))) (_RT_LESS x3 x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _590_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_RT_SOME (LAM (x2 (O (I I))) (_RT_LESS x2 x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ90 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_SOME (LAM (x1 (O (I I))) (_RT_LESS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _591_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (AND (_LESSF x2 x4) (_LESSF x4 x3)) (_RT_LESS x0 (_RATOF x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _591_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (AND (_LESSF x2 x4) (_LESSF x4 x3)) (_RT_LESS (_RATOF x4) x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _591_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (AND (_LESSF x2 x4) (_LESSF x4 x3)) (AND (_RT_LESS x0 (_RATOF x4)) (_RT_LESS (_RATOF x4) x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _591_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (AND (_LESSF x2 x4) (_LESSF x4 x3)) (_RT_SOME (LAM (x5 (O (I I))) (AND (_RT_LESS x0 x5) (_RT_LESS x5 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _591_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_SOME (LAM (x4 (O (I I))) (AND (_RT_LESS x0 x4) (_RT_LESS x4 x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ91 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESS x0 x1) (_RT_SOME (LAM (x2 (O (I I))) (AND (_RT_LESS x0 x2) (_RT_LESS x2 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PLUSFRT (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_RATOF (_PF x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PLUSFRT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (_RAT (_PLUSFRT x0 x2) (_PLUSFRT x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T18 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_RT_IS (_PLUSFRT x0 x2) (_PLUSFRT x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FPLUSFRT (in landau2)
 (definition (_FIXF _RAT _PLUSFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_PL (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_INDRAT x0 x1 _RAT _PLUSFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_PL_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_RAT (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T19 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_IS (_RATOF (_PF x2 x3)) (_RT_PL x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PICP (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_INF (_PF x2 x3) (_CLASS (_RT_PL x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISPL1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISPL2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_PL x2 x0) (_RT_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISPL12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_IS x2 x3) (_RT_IS (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _592_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_IS (_RT_PL x0 x1) (_RT_PL x1 x0)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ92 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_PL x0 x1) (_RT_PL x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_COMPL (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_PL x0 x1) (_RT_PL x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _593_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_INF (_PF (_PF x3 x4) x5) (_CLASS (_RT_PL (_RT_PL x0 x1) x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _593_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_INF (_PF x3 (_PF x4 x5)) (_CLASS (_RT_PL x0 (_RT_PL x1 x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _593_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_IS (_RT_PL (_RT_PL x0 x1) x2) (_RT_PL x0 (_RT_PL x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ93 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_PL (_RT_PL x0 x1) x2) (_RT_PL x0 (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ASSPL1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_PL (_RT_PL x0 x1) x2) (_RT_PL x0 (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ASSPL2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_PL x0 (_RT_PL x1 x2)) (_RT_PL (_RT_PL x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _594_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_MORE (_RT_PL x0 x1) x0))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ94 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_MORE (_RT_PL x0 x1) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ94A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_LESS x0 (_RT_PL x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _595_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ95 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _596_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ96A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _596_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_IS (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ96B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _596_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ96C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _596_ANDERSA (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _596_ANDERSB (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _596_ANDERSC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ96D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (_RT_MORE (_RT_PL x2 x0) (_RT_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ96E (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_PL x2 x0) (_RT_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ96F (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (_RT_LESS (_RT_PL x2 x0) (_RT_PL x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _597_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x2)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_MORE x0 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ97A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x2)) (_RT_MORE x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _597_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS (_RT_PL x0 x2) (_RT_PL x1 x2)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_IS x0 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ97B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS (_RT_PL x0 x2) (_RT_PL x1 x2)) (_RT_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _597_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x2)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESS x0 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ97C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x2)) (_RT_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _597_ANDERS (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x2)) (_RT_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _598_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MORE x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ98 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MORE x2 x3) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ98A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESS x2 x3) (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _599_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MORE x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ99A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MORE x2 x3) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _599_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ99B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (_RT_MORE (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ99C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESS x2 x3) (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ99D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESSIS x2 x3) (_RT_LESS (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5100_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MOREIS (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ100 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (_RT_MOREIS (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ100A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESSIS x2 x3) (_RT_LESSIS (_RT_PL x0 x2) (_RT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5101_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESSIS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS (_RT_PL x1 x2) x0) (_RT_MORE x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5101_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESSIS x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_NIS (_RT_PL x1 x2) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _VORBEMERKUNG101 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_LESSIS x0 x1) (NOT (_RT_SOME (LAM (x2 (O (I I))) (_RT_IS (_RT_PL x1 x2) x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5101_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (_EQ (_PF x3 x4) x2) (_RT_IS (_RT_PL x1 (_RATOF x4)) x0)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5101_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (_EQ (_PF x3 x4) x2) (_RT_SOME (LAM (x5 (O (I I))) (_RT_IS (_RT_PL x1 x5) x0)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5101_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_SOME (LAM (x4 (O (I I))) (_RT_IS (_RT_PL x1 x4) x0)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_SOME (LAM (x2 (O (I I))) (_RT_IS (_RT_PL x1 x2) x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5101_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS (_RT_PL x1 x2) x0) (IMPLIES (_RT_IS (_RT_PL x1 x3) x0) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_IS x2 x3))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS (_RT_PL x1 x2) x0) (IMPLIES (_RT_IS (_RT_PL x1 x3) x0) (_RT_IS x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5101_T7 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_AMONE _RAT (LAM (x2 (O (I I))) (_RT_IS (_RT_PL x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_ONE (LAM (x2 (O (I I))) (_RT_IS (_RT_PL x1 x2) x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_MN (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (THAT (LAM (x2 (O (I I))) (AND (_RAT x2 x2) (_RT_IS (_RT_PL x1 x2) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_MN_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (IMPLIES (_RT_MORE x0 x2) (_RAT (_RT_MN x0 x2) (_RT_MN x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_IS (_RT_PL x1 (_RT_MN x0 x1)) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_IS x0 (_RT_PL x1 (_RT_MN x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101E (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_IS (_RT_PL (_RT_MN x0 x1) x1) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101F (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_RT_MORE x0 x1) (_RT_IS x0 (_RT_PL (_RT_MN x0 x1) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ101G (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_IS (_RT_PL x1 x2) x0) (_RT_IS x2 (_RT_MN x0 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TIMESFRT (in landau2)
 (definition (LAM (x0 (I I)) (LAM (x1 (I I)) (_RATOF (_TF x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TIMESFRT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x0 x1) (FORALL (LAM (x2 (I I)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x2 x3) (_RAT (_TIMESFRT x0 x2) (_TIMESFRT x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T20 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_EQ x0 x1) (IMPLIES (_EQ x2 x3) (_RT_IS (_TIMESFRT x0 x2) (_TIMESFRT x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FTIMESFRT (in landau2)
 (definition (_FIXF _RAT _TIMESFRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_TS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_INDRAT x0 x1 _RAT _TIMESFRT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_TS_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_RAT (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T21 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_IS (_RATOF (_TF x2 x3)) (_RT_TS x0 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TICT (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_INF (_TF x2 x3) (_CLASS (_RT_TS x0 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISTS1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISTS2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_TS x2 x0) (_RT_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ISTS12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS x0 x1) (IMPLIES (_RT_IS x2 x3) (_RT_IS (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5102_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_IS (_RT_TS x0 x1) (_RT_TS x1 x0)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ102 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_TS x0 x1) (_RT_TS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_COMTS (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_TS x0 x1) (_RT_TS x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5103_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_INF (_TF (_TF x3 x4) x5) (_CLASS (_RT_TS (_RT_TS x0 x1) x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5103_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_INF (_TF x3 (_TF x4 x5)) (_CLASS (_RT_TS x0 (_RT_TS x1 x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5103_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_IS (_RT_TS (_RT_TS x0 x1) x2) (_RT_TS x0 (_RT_TS x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ103 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_TS (_RT_TS x0 x1) x2) (_RT_TS x0 (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ASSTS1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_TS (_RT_TS x0 x1) x2) (_RT_TS x0 (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_ASSTS2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_TS x0 (_RT_TS x1 x2)) (_RT_TS (_RT_TS x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5104_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_INF (_TF x3 (_PF x4 x5)) (_CLASS (_RT_TS x0 (_RT_PL x1 x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5104_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_INF (_PF (_TF x3 x4) (_TF x3 x5)) (_CLASS (_RT_PL (_RT_TS x0 x1) (_RT_TS x0 x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5104_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_IS (_RT_TS x0 (_RT_PL x1 x2)) (_RT_PL (_RT_TS x0 x1) (_RT_TS x0 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ104 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_TS x0 (_RT_PL x1 x2)) (_RT_PL (_RT_TS x0 x1) (_RT_TS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_DISTTP1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_TS (_RT_PL x0 x1) x2) (_RT_PL (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_DISTTP2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_TS x0 (_RT_PL x1 x2)) (_RT_PL (_RT_TS x0 x1) (_RT_TS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_DISTPT1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_PL (_RT_TS x0 x2) (_RT_TS x1 x2)) (_RT_TS (_RT_PL x0 x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RT_DISTPT2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (_RT_IS (_RT_PL (_RT_TS x0 x1) (_RT_TS x0 x2)) (_RT_TS x0 (_RT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5105_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ105A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5105_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_IS (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ105B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5105_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ105C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5105_ANDERSB (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5105_ANDERSC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ105D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x0 x1) (_RT_MORE (_RT_TS x2 x0) (_RT_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ105E (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS x0 x1) (_RT_IS (_RT_TS x2 x0) (_RT_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ105F (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS x0 x1) (_RT_LESS (_RT_TS x2 x0) (_RT_TS x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5106_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x2)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_MORE x0 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ106A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x2)) (_RT_MORE x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5106_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS (_RT_TS x0 x2) (_RT_TS x1 x2)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_IS x0 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ106B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS (_RT_TS x0 x2) (_RT_TS x1 x2)) (_RT_IS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5106_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x2)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (IMPLIES (_INF x3 (_CLASS x0)) (IMPLIES (_INF x4 (_CLASS x1)) (IMPLIES (_INF x5 (_CLASS x2)) (_RT_LESS x0 x1))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ106C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x2)) (_RT_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5106_ANDERS (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x2)) (_RT_LESS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5107_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MORE x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ107 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MORE x2 x3) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ107A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESS x2 x3) (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5108_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MORE x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ108A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MORE x2 x3) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5108_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ108B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MORE x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (_RT_MORE (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ108C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESS x2 x3) (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ108D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESS x0 x1) (IMPLIES (_RT_LESSIS x2 x3) (_RT_LESS (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5109_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_MOREIS (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ109 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_MOREIS x0 x1) (IMPLIES (_RT_MOREIS x2 x3) (_RT_MOREIS (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ109A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_LESSIS x0 x1) (IMPLIES (_RT_LESSIS x2 x3) (_RT_LESSIS (_RT_TS x0 x2) (_RT_TS x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5110_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (_EQ (_TF x3 x4) x2) (_RT_IS (_RT_TS x1 (_RATOF x4)) x0))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5110_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (IMPLIES (_EQ (_TF x3 x4) x2) (_RT_SOME (LAM (x5 (O (I I))) (_RT_IS (_RT_TS x1 x5) x0))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5110_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_FRAC x2 x2) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x2 (_CLASS x0)) (IMPLIES (_INF x3 (_CLASS x1)) (_RT_SOME (LAM (x4 (O (I I))) (_RT_IS (_RT_TS x1 x4) x0))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_SOME (LAM (x2 (O (I I))) (_RT_IS (_RT_TS x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5110_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS (_RT_TS x1 x2) x0) (IMPLIES (_RT_IS (_RT_TS x1 x3) x0) (FORALL (LAM (x4 (I I)) (IMPLIES (_FRAC x4 x4) (FORALL (LAM (x5 (I I)) (IMPLIES (_FRAC x5 x5) (FORALL (LAM (x6 (I I)) (IMPLIES (_FRAC x6 x6) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x7 x7) (IMPLIES (_INF x4 (_CLASS x0)) (IMPLIES (_INF x5 (_CLASS x1)) (IMPLIES (_INF x6 (_CLASS x2)) (IMPLIES (_INF x7 (_CLASS x3)) (_RT_IS x2 x3))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x3 x3) (IMPLIES (_RT_IS (_RT_TS x1 x2) x0) (IMPLIES (_RT_IS (_RT_TS x1 x3) x0) (_RT_IS x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5110_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_AMONE _RAT (LAM (x2 (O (I I))) (_RT_IS (_RT_TS x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_ONE (LAM (x2 (O (I I))) (_RT_IS (_RT_TS x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5111_T1 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES (_NUM (_FR x0 _1)) (_DEN (_FR x1 _1))) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5111_T2 (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= x0 (_TIMES (_NUM (_FR x0 _1)) (_DEN (_FR x1 _1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ111A (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MOREF (_FR x0 _1) (_FR x1 _1)) (_MORE x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ111B (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_EQ (_FR x0 _1) (_FR x1 _1)) (= x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ111C (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSF (_FR x0 _1) (_FR x1 _1)) (_LESS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ111D (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (_MOREF (_FR x0 _1) (_FR x1 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ111E (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_EQ (_FR x0 _1) (_FR x1 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ111F (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x0 x1) (_LESSF (_FR x0 _1) (_FR x1 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATPROP (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 I) (_INF (_FR x1 _1) (_CLASS x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATRT (in landau2)
 (definition (LAM (x0 (O (I I))) (_N_SOME (_NATPROP x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T22 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_NATPROP x0 x2) (IMPLIES (_NATPROP x1 x3) (IMPLIES (_RT_IS x0 x1) (= x2 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T23 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_AMONE = (_NATPROP x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ111G (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (_N_ONE (_NATPROP x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOFRT (in landau2)
 (definition (LAM (x0 (O (I I))) (THAT (_NATPROP x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOFRT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (IMPLIES (_NATRT x0) (= (_NOFRT x0) (_NOFRT x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INCLASSN (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (_INF (_FR (_NOFRT x0) _1) (_CLASS x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTEN (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_IS x0 x1) (= (_NOFRT x0) (_NOFRT x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTIN (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (= (_NOFRT x0) (_NOFRT x1)) (_RT_IS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFN (in landau2)
 (definition (LAM (x0 I) (_RATOF (_FR x0 _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFN_DEC (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_RAT (_RTOFN x0) (_RTOFN x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATRTI (in landau2)
 (definition (FORALL (LAM (x0 I) (_NATRT (_RTOFN x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNERT (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_RT_IS (_RTOFN x0) (_RTOFN x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNIRT (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_RT_IS (_RTOFN x0) (_RTOFN x1)) (= x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTN1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (_RT_IS x0 (_RTOFN (_NOFRT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNRT1 (in landau2)
 (definition (FORALL (LAM (x0 I) (= x0 (_NOFRT (_RTOFN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112A (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_EQ (_PF (_FR x0 _1) (_FR x1 _1)) (_FR (_PL x0 x1) _1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112B (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_EQ (_TF (_FR x0 _1) (_FR x1 _1)) (_FR (_TIMES x0 x1) _1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (_INF (_FR (_PL (_NOFRT x0) (_NOFRT x1)) _1) (_CLASS (_RT_PL x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (_NATRT (_RT_PL x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112E (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (_INF (_FR (_TIMES (_NOFRT x0) (_NOFRT x1)) _1) (_CLASS (_RT_TS x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112F (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (_NATRT (_RT_TS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5112_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_MORE x0 x1) (_MORE (_NOFRT x0) (_NOFRT x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5112_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP (_NOFRT x0) (_NOFRT x1) x2) (= (_NOFRT x0) (_PL (_NOFRT x1) (_NOFRT (_RTOFN x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5112_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP (_NOFRT x0) (_NOFRT x1) x2) (_RT_IS x0 (_RT_PL x1 (_RTOFN x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5112_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP (_NOFRT x0) (_NOFRT x1) x2) (_RT_IS (_RTOFN x2) (_RT_MN x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5112_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_MORE x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP (_NOFRT x0) (_NOFRT x1) x2) (_NATRT (_RT_MN x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112G (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_MORE x0 x1) (_NATRT (_RT_MN x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112H (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_RT_IS (_RT_PL (_RTOFN x0) (_RTOFN x1)) (_RTOFN (_PL x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ112J (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_RT_IS (_RT_TS (_RTOFN x0) (_RTOFN x1)) (_RTOFN (_TIMES x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATT (in landau2)
 (definition (_OT _RAT _NATRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NATT_DEC (in landau2)
 (definition (PER _NATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFRT (in landau2)
 (definition (_OUT _RAT _NATRT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFRT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (IMPLIES (_NATRT x0) (_NATT (_NTOFRT x0) (_NTOFRT x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_IS (in landau2)
 (definition _NATT)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_NIS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (NOT (_NT_IS x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_ALL (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SOME (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (EXISTS (LAM (x1 (O (I I))) (AND (_NATT x1 x1) (x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_ONE (in landau2)
 (definition (_E_ONE _NATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_IN (in landau2)
 (definition (_ESTI _NATT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFNT (in landau2)
 (definition (LAM (x0 (O (I I))) x0))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RTOFNT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x0 x1) (_RAT (_RTOFNT x0) (_RTOFNT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_NATRTI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_NATRT (_RTOFNT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTENT (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_RT_IS x0 x1) (_NT_IS (_NTOFRT x0) (_NTOFRT x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTINT (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (IMPLIES (_NATRT x1) (IMPLIES (_NT_IS (_NTOFRT x0) (_NTOFRT x1)) (_RT_IS x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTERT (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_IS x0 x1) (_RT_IS (_RTOFNT x0) (_RTOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTIRT (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_RT_IS (_RTOFNT x0) (_RTOFNT x1)) (_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISRTNT1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (IMPLIES (_NATRT x0) (_RT_IS x0 (_RTOFNT (_NTOFRT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTRT1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_NT_IS x0 (_NTOFRT (_RTOFNT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFN (in landau2)
 (definition (LAM (x0 I) (_NTOFRT (_RTOFN x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NTOFN_DEC (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_NATT (_NTOFN x0) (_NTOFN x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNENT (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_NT_IS (_NTOFN x0) (_NTOFN x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNINT (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_NT_IS (_NTOFN x0) (_NTOFN x1)) (= x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOFNT (in landau2)
 (definition (LAM (x0 (O (I I))) (_NOFRT (_RTOFNT x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOFNT_DEC (in landau2)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTEN (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_IS x0 x1) (= (_NOFNT x0) (_NOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTIN (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (= (_NOFNT x0) (_NOFNT x1)) (_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T24 (in landau2)
 (definition (FORALL (LAM (x0 I) (_RT_IS (_RTOFN x0) (_RTOFNT (_NTOFN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T25 (in landau2)
 (definition (FORALL (LAM (x0 I) (= (_NOFRT (_RTOFN x0)) (_NOFNT (_NTOFN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNNT1 (in landau2)
 (definition (FORALL (LAM (x0 I) (= x0 (_NOFNT (_NTOFN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T26 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_RT_IS (_RTOFNT x0) (_RTOFN (_NOFNT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T27 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_NT_IS (_NTOFRT (_RTOFNT x0)) (_NTOFN (_NOFNT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTN1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_NT_IS x0 (_NTOFN (_NOFNT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNNT2 (in landau2)
 (definition (FORALL (LAM (x0 I) (= (_NOFNT (_NTOFN x0)) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTN2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_NT_IS (_NTOFN (_NOFNT x0)) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_1T (in landau2)
 (definition (_NTOFN _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_1T_DEC (in landau2)
 (definition (_NATT _NT_1T _NT_1T))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUCT (in landau2)
 (definition (LAM (x0 (O (I I))) (_NTOFN (_SUC (_NOFNT x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SUCT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x0 x1) (_NATT (_SUCT x0) (_SUCT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5113_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (IMPLIES (_NT_IS (_SUCT x0) _NT_1T) (= (_SUC (_NOFNT x0)) _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ113A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_NT_NIS (_SUCT x0) _NT_1T)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5113_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_IS (_SUCT x0) (_SUCT x1)) (= (_SUC (_NOFNT x0)) (_SUC (_NOFNT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ113B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_IS (_SUCT x0) (_SUCT x1)) (_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_COND1 (in landau2)
 (definition (_NT_IN _NT_1T))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_COND2 (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (_NT_ALL (LAM (x1 (O (I I))) (IMPLIES (_NT_IN x1 x0) (_NT_IN (_SUCT x1) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5113_PROP1 (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 I) (_NT_IN (_NTOFN x1) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5113_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 I) (IMPLIES (_5113_PROP1 x0 x1) (IMPLIES (_NT_IN (_NTOFN x1) x0) (_NT_IN (_SUCT (_NTOFN x1)) x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5113_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 I) (IMPLIES (_5113_PROP1 x0 x1) (_NT_IN (_SUCT (_NTOFN x1)) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5113_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 I) (IMPLIES (_5113_PROP1 x0 x1) (_5113_PROP1 x0 (_SUC x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5113_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_NT_IN (_NTOFN (_NOFNT x1)) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ113C (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_NT_IN x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AX3T (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (_NT_NIS (_SUCT x0) _NT_1T)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AX4T (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_IS (_SUCT x0) (_SUCT x1)) (_NT_IS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AX5T (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_NT_IN x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _51_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_NIS x0 x1) (NOT (= (_NOFNT x0) (_NOFNT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _51_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_NIS x0 x1) (NOT (= (_SUC (_NOFNT x0)) (_SUC (_NOFNT x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SATZ1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_NIS x0 x1) (_NT_NIS (_SUCT x0) (_SUCT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X (in landau2)
 (definition (LAM (x0 (O (O (I I)))) _NOFNT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _X_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x1) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (= (_X x0) (_X x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROP1T (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I) (O (I I)))) (_NT_ALL (LAM (x3 (O (I I))) (_NT_IS (x2 (_SUCT x3)) (_SUCT (x2 x3)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROP2T (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I) (O (I I)))) (AND (_NT_IS (x2 _NT_1T) (_SUCT x1)) (_PROP1T x0 x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_PROP1 (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (I I)) (_N_ALL (LAM (x3 I) (= (x2 (_SUC x3)) (_SUC (x2 x3)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_PROP2 (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (I I)) (AND (= (x2 _1) (_SUC (_X x0 x1))) (_54_PROP1 x0 x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_G (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I) (O (I I)))) (LAM (x3 I) (_NOFNT (x2 (_NTOFN x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_G_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x1) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (= (_54_G x0) (_54_G x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (_NT_IS (x2 _NT_1T) (_SUCT x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (= (_54_G x0 x1 x2 _1) (_SUC (_X x0 x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (_PROP1T x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UT (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I) (O (I I)))) _NTOFN))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x1) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x2 x3) (FORALL (LAM (x4 (O (I I) (O (I I)))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x4 x6) (x5 x7))))))) (IMPLIES (_PROP2T x0 x2 x4) (FORALL (LAM (x8 I) (FORALL (LAM (x9 I) (IMPLIES (= x8 x9) (_NATT (_UT x0 x2 x4 x8) (_UT x1 x3 x5 x9))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (FORALL (LAM (x5 I) (= (_SUC x5) (_SUC (_NOFNT (_UT x0 x1 x2 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (FORALL (LAM (x5 I) (= (_54_G x0 x1 x2 (_SUC x5)) (_NOFNT (x2 (_SUCT (_UT x0 x1 x2 x5))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (FORALL (LAM (x5 I) (_NT_IS (x2 (_SUCT (_UT x0 x1 x2 x5))) (_SUCT (x2 (_UT x0 x1 x2 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T7 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (FORALL (LAM (x5 I) (= (_NOFNT (x2 (_SUCT (_UT x0 x1 x2 x5)))) (_NOFNT (_SUCT (x2 (_UT x0 x1 x2 x5))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T8 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (FORALL (LAM (x5 I) (= (_NOFNT (_SUCT (x2 (_UT x0 x1 x2 x5)))) (_SUC (_54_G x0 x1 x2 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T9 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (FORALL (LAM (x5 I) (= (_54_G x0 x1 x2 (_SUC x5)) (_SUC (_54_G x0 x1 x2 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T10 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (_54_PROP1 x0 x1 (_54_G x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T11 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (IMPLIES (_PROP2T x0 x1 x2) (_54_PROP2 x0 x1 (_54_G x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T12 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x5 x6) (x5 x7))))))) (IMPLIES (_PROP2T x0 x1 x2) (IMPLIES (_PROP2T x0 x1 x5) (_AMONE = (_54_PROP2 x0 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T13 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x5 x6) (x5 x7))))))) (IMPLIES (_PROP2T x0 x1 x2) (IMPLIES (_PROP2T x0 x1 x5) (= (_54_G x0 x1 x2) (_54_G x0 x1 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Y (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I) (O (I I)))) (LAM (x3 (O (I I) (O (I I)))) _NOFNT)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Y_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x1) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x2 x3) (FORALL (LAM (x4 (O (I I) (O (I I)))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x4 x6) (x5 x7))))))) (FORALL (LAM (x8 (O (I I) (O (I I)))) (FORALL (LAM (x9 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x10 (O (I I))) (FORALL (LAM (x11 (O (I I))) (IMPLIES (_NATT x10 x11) (_NATT (x8 x10) (x9 x11))))))) (IMPLIES (_PROP2T x0 x2 x4) (IMPLIES (_PROP2T x0 x2 x8) (= (_Y x0 x2 x4 x8) (_Y x1 x3 x5 x9)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T14 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x5 x6) (x5 x7))))))) (IMPLIES (_PROP2T x0 x1 x2) (IMPLIES (_PROP2T x0 x1 x5) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_NATT x8 x8) (= (_NOFNT (x2 (_NTOFN (_Y x0 x1 x2 x5 x8)))) (_NOFNT (x5 (_NTOFN (_Y x0 x1 x2 x5 x8)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T15 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x5 x6) (x5 x7))))))) (IMPLIES (_PROP2T x0 x1 x2) (IMPLIES (_PROP2T x0 x1 x5) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_NATT x8 x8) (_NT_IS (x2 (_NTOFN (_Y x0 x1 x2 x5 x8))) (x5 (_NTOFN (_Y x0 x1 x2 x5 x8))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T16 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x5 x6) (x5 x7))))))) (IMPLIES (_PROP2T x0 x1 x2) (IMPLIES (_PROP2T x0 x1 x5) (FORALL (LAM (x8 (O (I I))) (IMPLIES (_NATT x8 x8) (_NT_IS (x2 x8) (x5 x8))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T17 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (O (I I) (O (I I)))) (IMPLIES (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (x5 x6) (x5 x7))))))) (IMPLIES (_PROP2T x0 x1 x2) (IMPLIES (_PROP2T x0 x1 x5) (FORALL (LAM (x8 (O (I I))) (FORALL (LAM (x9 (O (I I))) (IMPLIES (_NATT x8 x9) (_NATT (x2 x8) (x5 x9))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T18 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_AMONE (LAM (x2 (O (I I) (O (I I)))) (LAM (x3 (O (I I) (O (I I)))) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_NATT x4 x5) (_NATT (x2 x4) (x3 x5))))))))) (_PROP2T x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T19 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (EXISTS (LAM (x2 (I I)) (AND (= x2 x2) (_54_PROP2 x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _GT (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (I I)) (LAM (x3 (O (I I))) (_NTOFN (x2 (_NOFNT x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _GT_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x1) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x2 x3) (FORALL (LAM (x4 (I I)) (FORALL (LAM (x5 (I I)) (IMPLIES (= x4 x5) (FORALL (LAM (x6 (O (I I))) (FORALL (LAM (x7 (O (I I))) (IMPLIES (_NATT x6 x7) (_NATT (_GT x0 x2 x4 x6) (_GT x1 x3 x5 x7)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T20 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (= (x2 _1) (_SUC (_X x0 x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T21 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (= (x2 (_NOFNT _NT_1T)) (x2 _1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T22 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (_NT_IS (_GT x0 x1 x2 _NT_1T) (_SUCT x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T23 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (_54_PROP1 x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Z (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (LAM (x2 (I I)) _NOFNT))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Z_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (FORALL (LAM (x1 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x1) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x2 x3) (FORALL (LAM (x4 (I I)) (FORALL (LAM (x5 (I I)) (IMPLIES (= x4 x5) (IMPLIES (_54_PROP2 x0 x2 x4) (= (_Z x0 x2 x4) (_Z x1 x3 x5)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T24 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x3 x3) (= (x2 (_NOFNT (_SUCT x3))) (x2 (_SUC (_Z x0 x1 x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T25 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x3 x3) (= (x2 (_SUC (_Z x0 x1 x2 x3))) (_SUC (x2 (_Z x0 x1 x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T26 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x3 x3) (= (_SUC (x2 (_Z x0 x1 x2 x3))) (_SUC (_NOFNT (_GT x0 x1 x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T27 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x3 x3) (_NT_IS (_GT x0 x1 x2 (_SUCT x3)) (_SUCT (_GT x0 x1 x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T28 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (_PROP1T x0 x1 (_GT x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T29 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (_PROP2T x0 x1 (_GT x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T30 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (I I)) (IMPLIES (_54_PROP2 x0 x1 x2) (EXISTS (LAM (x3 (O (I I) (O (I I)))) (AND (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_NATT x4 x5) (_NATT (x3 x4) (x3 x5))))))) (_PROP2T x0 x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _54_T31 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (EXISTS (LAM (x2 (O (I I) (O (I I)))) (AND (FORALL (LAM (x3 (O (I I))) (FORALL (LAM (x4 (O (I I))) (IMPLIES (_NATT x3 x4) (_NATT (x2 x3) (x2 x4))))))) (_PROP2T x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SATZ4 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_SET _NATT x0 x0) (IMPLIES (_NT_COND1 x0) (IMPLIES (_NT_COND2 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_E_ONE (LAM (x2 (O (I I) (O (I I)))) (LAM (x3 (O (I I) (O (I I)))) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_NATT x4 x5) (_NATT (x2 x4) (x3 x5))))))))) (LAM (x6 (O (I I) (O (I I)))) (AND (_NT_IS (x6 _NT_1T) (_SUCT x1)) (_NT_ALL (LAM (x7 (O (I I))) (_NT_IS (x6 (_SUCT x7)) (_SUCT (x6 x7)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_PL (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_NTOFRT (_RT_PL (_RTOFNT x0) (_RTOFNT x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_PL_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x2 x3) (_NATT (_NT_PL x0 x2) (_NT_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T28 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_INF (_FR (_PL (_NOFNT x0) (_NOFNT x1)) _1) (_CLASS (_RT_PL (_RTOFNT x0) (_RTOFNT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T29 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_RT_IS (_RT_PL (_RTOFNT x0) (_RTOFNT x1)) (_RTOFN (_PL (_NOFNT x0) (_NOFNT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISPLNT (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_NT_IS (_NT_PL x0 x1) (_NTOFN (_PL (_NOFNT x0) (_NOFNT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNTPL (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (_NT_IS (_NTOFN (_PL (_NOFNT x0) (_NOFNT x1))) (_NT_PL x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISPLN (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (= (_PL (_NOFNT x0) (_NOFNT x1)) (_NOFNT (_NT_PL x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISNPL (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (= (_NOFNT (_NT_PL x0 x1)) (_PL (_NOFNT x0) (_NOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _55_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (= (_PL (_NOFNT (_NT_PL x0 x1)) (_NOFNT x2)) (_PL (_PL (_NOFNT x0) (_NOFNT x1)) (_NOFNT x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _55_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (= (_PL (_NOFNT x0) (_PL (_NOFNT x1) (_NOFNT x2))) (_PL (_NOFNT x0) (_NOFNT (_NT_PL x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _55_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (= (_PL (_NOFNT (_NT_PL x0 x1)) (_NOFNT x2)) (_PL (_NOFNT x0) (_NOFNT (_NT_PL x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SATZ5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (_NT_IS (_NT_PL (_NT_PL x0 x1) x2) (_NT_PL x0 (_NT_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_DIFFPROP (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) (_NT_IS x0 (_NT_PL x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFFPROPE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (_NT_DIFFPROP x0 x1 x2) (_DIFFPROP (_NOFNT x0) (_NOFNT x1) (_NOFNT x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T30 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (_DIFFPROP (_NOFNT x0) (_NOFNT x1) (_NOFNT x2)) (= (_NOFNT x0) (_NOFNT (_NT_PL x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFFPROPI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (_DIFFPROP (_NOFNT x0) (_NOFNT x1) (_NOFNT x2)) (_NT_DIFFPROP x0 x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IT (in landau2)
 (definition _NT_IS)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIT (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_NT_SOME (_NT_DIFFPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IIIT (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_NT_SOME (_NT_DIFFPROP x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_I (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (= (_NOFNT x0) (_NOFNT x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_II (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_N_SOME (_DIFFPROP (_NOFNT x0) (_NOFNT x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_III (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_N_SOME (_DIFFPROP (_NOFNT x1) (_NOFNT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_59_I x0 x1) (OR (_IT x0 x1) (OR (_IIT x0 x1) (_IIIT x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_59_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP (_NOFNT x0) (_NOFNT x1) x2) (_DIFFPROP (_NOFNT x0) (_NOFNT x1) (_NOFNT (_NTOFN x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_59_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP (_NOFNT x0) (_NOFNT x1) x2) (_IIT x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_59_II x0 x1) (_IIT x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_59_II x0 x1) (OR (_IT x0 x1) (OR (_IIT x0 x1) (_IIIT x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_59_III x0 x1) (OR (_IT x0 x1) (OR (_IIT x0 x1) (_IIIT x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T7 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (OR (_IT x0 x1) (OR (_IIT x0 x1) (_IIIT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T8 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IT x0 x1) (_59_I x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T9 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIT x0 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (_NT_DIFFPROP x0 x1 x2) (_59_II x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T10 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIT x0 x1) (_59_II x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T11 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIIT x0 x1) (_59_III x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (AND (IMPLIES (_59_I x0 x1) (NOT (_59_II x0 x1))) (AND (IMPLIES (_59_II x0 x1) (NOT (_59_III x0 x1))) (IMPLIES (_59_III x0 x1) (NOT (_59_I x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T13 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IT x0 x1) (NOT (_59_II x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T14 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IT x0 x1) (NOT (_IIT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T15 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IT x0 x1) (NOT (_IIT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T16 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIT x0 x1) (NOT (_59_III x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T17 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIT x0 x1) (NOT (_IIIT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T18 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIT x0 x1) (NOT (_IIIT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T19 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIIT x0 x1) (NOT (_59_I x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T20 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIIT x0 x1) (NOT (_IT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T21 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_IIIT x0 x1) (NOT (_IT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _59_T22 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (AND (IMPLIES (_IT x0 x1) (NOT (_IIT x0 x1))) (AND (IMPLIES (_IIT x0 x1) (NOT (_IIIT x0 x1))) (IMPLIES (_IIIT x0 x1) (NOT (_IT x0 x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SATZ9 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (AND (OR (_NT_IS x0 x1) (OR (_NT_SOME (LAM (x2 (O (I I))) (_NT_IS x0 (_NT_PL x1 x2)))) (_NT_SOME (LAM (x3 (O (I I))) (_NT_IS x1 (_NT_PL x0 x3)))))) (AND (IMPLIES (_NT_IS x0 x1) (NOT (_NT_SOME (LAM (x4 (O (I I))) (_NT_IS x0 (_NT_PL x1 x4)))))) (AND (IMPLIES (_NT_SOME (LAM (x5 (O (I I))) (_NT_IS x0 (_NT_PL x1 x5)))) (NOT (_NT_SOME (LAM (x6 (O (I I))) (_NT_IS x1 (_NT_PL x0 x6)))))) (IMPLIES (_NT_SOME (LAM (x7 (O (I I))) (_NT_IS x1 (_NT_PL x0 x7)))) (NOT (_NT_IS x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_MORE (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_RT_MORE (_RTOFNT x0) (_RTOFNT x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T31 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_MORE x0 x1) (_MOREF (_FR (_NOFNT x0) _1) (_FR (_NOFNT x1) _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_MOREE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_MORE x0 x1) (_MORE (_NOFNT x0) (_NOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T32 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_MORE (_NOFNT x0) (_NOFNT x1)) (_MOREF (_FR (_NOFNT x0) _1) (_FR (_NOFNT x1) _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_MOREI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_MORE (_NOFNT x0) (_NOFNT x1)) (_NT_MORE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_LESS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_RT_LESS (_RTOFNT x0) (_RTOFNT x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T33 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_LESS x0 x1) (_LESSF (_FR (_NOFNT x0) _1) (_FR (_NOFNT x1) _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_LESSE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_LESS x0 x1) (_LESS (_NOFNT x0) (_NOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T34 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_LESS (_NOFNT x0) (_NOFNT x1)) (_LESSF (_FR (_NOFNT x0) _1) (_FR (_NOFNT x1) _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_LESSI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_LESS (_NOFNT x0) (_NOFNT x1)) (_NT_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_MOREIS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_RT_MOREIS (_RTOFNT x0) (_RTOFNT x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_MOREISE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_MOREIS x0 x1) (_MOREIS (_NOFNT x0) (_NOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_MOREISI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_MOREIS (_NOFNT x0) (_NOFNT x1)) (_NT_MOREIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_LESSIS (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (_RT_LESSIS (_RTOFNT x0) (_RTOFNT x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_LESSISE (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_NT_LESSIS x0 x1) (_LESSIS (_NOFNT x0) (_NOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_LESSISI (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (_LESSIS (_NOFNT x0) (_NOFNT x1)) (_NT_LESSIS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _515_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (_NT_LESS x0 x1) (IMPLIES (_NT_LESS x1 x2) (_LESS (_NOFNT x0) (_NOFNT x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SATZ15 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (_NT_LESS x0 x1) (IMPLIES (_NT_LESS x1 x2) (_NT_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _521_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x3 x3) (IMPLIES (_NT_MORE x0 x1) (IMPLIES (_NT_MORE x2 x3) (_MORE (_PL (_NOFNT x0) (_NOFNT x2)) (_PL (_NOFNT x1) (_NOFNT x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _521_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x3 x3) (IMPLIES (_NT_MORE x0 x1) (IMPLIES (_NT_MORE x2 x3) (_MORE (_NOFNT (_NT_PL x0 x2)) (_NOFNT (_NT_PL x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SATZ21 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_NATT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_NATT x3 x3) (IMPLIES (_NT_MORE x0 x1) (IMPLIES (_NT_MORE x2 x3) (_NT_MORE (_NT_PL x0 x2) (_NT_PL x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_LB (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (_NT_ALL (LAM (x2 (O (I I))) (IMPLIES (x0 x2) (_NT_LESSIS x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_MIN (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 (O (I I))) (AND (_NT_LB x0 x1) (x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _Q (in landau2)
 (definition (LAM (x0 (O (O (I I)))) (LAM (x1 I) (x0 (_NTOFN x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (x0 x1) (_Q x0 (_NOFNT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_NATT x1 x1) (IMPLIES (x0 x1) (_N_SOME (_Q x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (_N_SOME (_Q x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (_N_SOME (_MIN (_Q x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (_LB (_Q x0) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (FORALL (LAM (x2 I) (IMPLIES (_Q x0 x2) (_LESSIS x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T7 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (x0 x2) (_LESSIS x1 (_NOFNT x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T8 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (x0 x2) (_LESSIS (_NOFNT (_NTOFN x1)) (_NOFNT x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T9 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (x0 x2) (_NT_LESSIS (_NTOFN x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T10 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_NATT x2 x2) (IMPLIES (x0 x2) (_NT_LESSIS (_NTOFN x1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T11 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (_NT_LB x0 (_NTOFN x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T12 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (x0 (_NTOFN x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T13 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (_NT_MIN x0 (_NTOFN x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _527_T14 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (_MIN (_Q x0) x1) (_NT_SOME (_NT_MIN x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NT_SATZ27 (in landau2)
 (definition (FORALL (LAM (x0 (O (O (I I)))) (IMPLIES (_NT_SOME x0) (_NT_SOME (_NT_MIN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1RT (in landau2)
 (definition (_RTOFN _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1RT_DEC (in landau2)
 (definition (_RAT _1RT _1RT))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T35 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_EQ (_TF x1 (_FR _1 _1)) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II5_T36 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_FRAC x1 x1) (IMPLIES (_INF x1 (_CLASS x0)) (_RT_IS (_RT_TS x0 _1RT) x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EXAMPLE1A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS (_RT_TS x0 _1RT) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EXAMPLE1B (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS x0 (_RT_TS x0 _1RT))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EXAMPLE1C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS (_RT_TS _1RT x0) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EXAMPLE1D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (_RT_IS x0 (_RT_TS _1RT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5114_T1 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_EQ (_TF (_FR (_DEN x0) _1) x0) (_FR (_NUM x0) _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ114 (in landau2)
 (definition (FORALL (LAM (x0 (I I)) (IMPLIES (_FRAC x0 x0) (_RT_IS (_RT_TS (_RTOFN (_DEN x0)) (_RATOF x0)) (_RTOFN (_NUM x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ114A (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_RT_IS (_RT_TS (_RTOFN x1) (_RATOF (_FR x0 x1))) (_RTOFN x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OV (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (THAT (LAM (x2 (O (I I))) (AND (_RAT x2 x2) (_RT_IS (_RT_TS x1 x2) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OV_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (_RAT (_OV x0 x2) (_OV x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110C (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_TS x1 (_OV x0 x1)) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110D (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS x0 (_RT_TS x1 (_OV x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110E (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS (_RT_TS (_OV x0 x1) x1) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110F (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_IS x0 (_RT_TS (_OV x0 x1) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ110G (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_IS (_RT_TS x1 x2) x0) (_RT_IS x2 (_OV x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ114B (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_RT_IS (_RATOF (_FR x0 x1)) (_OV (_RTOFN x0) (_RTOFN x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ114C (in landau2)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_RT_IS (_OV (_RTOFN x0) (_RTOFN x1)) (_RATOF (_FR x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T1 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_SOME (LAM (x2 (O (I I))) (_RT_MORE x2 (_OV x1 x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_Z (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) _NUM))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_Z_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RT_MORE x4 (_OV x2 x0)) (FORALL (LAM (x6 (I I)) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x6 x7) (IMPLIES (_INF x6 (_CLASS x4)) (= (_5115_Z x0 x2 x4 x6) (_5115_Z x1 x3 x5 x7)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_V (in landau2)
 (definition (LAM (x0 (O (I I))) (LAM (x1 (O (I I))) (LAM (x2 (O (I I))) _DEN))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_V_DEC (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x0 x1) (FORALL (LAM (x2 (O (I I))) (FORALL (LAM (x3 (O (I I))) (IMPLIES (_RAT x2 x3) (FORALL (LAM (x4 (O (I I))) (FORALL (LAM (x5 (O (I I))) (IMPLIES (_RAT x4 x5) (IMPLIES (_RT_MORE x4 (_OV x2 x0)) (FORALL (LAM (x6 (I I)) (FORALL (LAM (x7 (I I)) (IMPLIES (_FRAC x6 x7) (IMPLIES (_INF x6 (_CLASS x4)) (= (_5115_V x0 x2 x4 x6) (_5115_V x1 x3 x5 x7)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T2 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_RT_MORE (_OV (_RTOFN (_5115_Z x0 x1 x2 x3)) (_RTOFN (_5115_V x0 x1 x2 x3))) (_OV x1 x0)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T3 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_RT_MOREIS (_RTOFN (_5115_V x0 x1 x2 x3)) _1RT))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T4 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_RT_IS (_RT_TS (_RTOFN (_5115_Z x0 x1 x2 x3)) x0) (_RT_TS (_RT_TS x0 (_OV (_RTOFN (_5115_Z x0 x1 x2 x3)) (_RTOFN (_5115_V x0 x1 x2 x3)))) (_RTOFN (_5115_V x0 x1 x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T5 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (IMPLIES (_RT_MORE (_RTOFN (_5115_V x0 x1 x2 x3)) _1RT) (_RT_MORE (_RT_TS (_RTOFN (_5115_Z x0 x1 x2 x3)) x0) (_RT_TS (_RT_TS x0 (_OV (_RTOFN (_5115_Z x0 x1 x2 x3)) (_RTOFN (_5115_V x0 x1 x2 x3)))) _1RT))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T6 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (IMPLIES (_RT_MORE (_RTOFN (_5115_V x0 x1 x2 x3)) _1RT) (_RT_MOREIS (_RT_TS (_RTOFN (_5115_Z x0 x1 x2 x3)) x0) (_RT_TS (_RT_TS x0 (_OV (_RTOFN (_5115_Z x0 x1 x2 x3)) (_RTOFN (_5115_V x0 x1 x2 x3)))) _1RT))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T7 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (IMPLIES (_RT_IS (_RTOFN (_5115_V x0 x1 x2 x3)) _1RT) (_RT_MOREIS (_RT_TS (_RTOFN (_5115_Z x0 x1 x2 x3)) x0) (_RT_TS (_RT_TS x0 (_OV (_RTOFN (_5115_Z x0 x1 x2 x3)) (_RTOFN (_5115_V x0 x1 x2 x3)))) _1RT))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T8 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_RT_MOREIS (_RT_TS (_RTOFN (_5115_Z x0 x1 x2 x3)) x0) (_RT_TS (_RT_TS x0 (_OV (_RTOFN (_5115_Z x0 x1 x2 x3)) (_RTOFN (_5115_V x0 x1 x2 x3)))) _1RT)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T9 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_RT_MORE (_RT_TS (_RT_TS x0 (_OV (_RTOFN (_5115_Z x0 x1 x2 x3)) (_RTOFN (_5115_V x0 x1 x2 x3)))) _1RT) x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T10 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_RT_MORE (_RT_TS (_RTOFN (_5115_Z x0 x1 x2 x3)) x0) x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T11 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_N_SOME (LAM (x4 I) (_RT_MORE (_RT_TS (_RTOFN x4) x0) x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T12 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (_N_SOME (LAM (x3 I) (_RT_MORE (_RT_TS (_RTOFN x3) x0) x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ115 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_N_SOME (LAM (x2 I) (_RT_MORE (_RT_TS (_RTOFN x2) x0) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T13 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (AND (_NATRT (_RTOFN (_5115_Z x0 x1 x2 x3))) (_RT_MORE (_RT_TS (_RTOFN (_5115_Z x0 x1 x2 x3)) x0) x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T14 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (FORALL (LAM (x3 (I I)) (IMPLIES (_FRAC x3 x3) (IMPLIES (_INF x3 (_CLASS x2)) (_RT_SOME (LAM (x4 (O (I I))) (AND (_NATRT x4) (_RT_MORE (_RT_TS x4 x0) x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5115_T15 (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (FORALL (LAM (x2 (O (I I))) (IMPLIES (_RAT x2 x2) (IMPLIES (_RT_MORE x2 (_OV x1 x0)) (_RT_SOME (LAM (x3 (O (I I))) (AND (_NATRT x3) (_RT_MORE (_RT_TS x3 x0) x1)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ115A (in landau2)
 (definition (FORALL (LAM (x0 (O (I I))) (IMPLIES (_RAT x0 x0) (FORALL (LAM (x1 (O (I I))) (IMPLIES (_RAT x1 x1) (_RT_SOME (LAM (x2 (O (I I))) (AND (_NATRT x2) (_RT_MORE (_RT_TS x2 x0) x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

