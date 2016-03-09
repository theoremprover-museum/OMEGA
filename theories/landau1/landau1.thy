; Chad E Brown; In November 2005 I translated Jutting's Automath version of
; Landau's Grundlagen der Analysis into TPS (without proofs) and then
; from TPS into this post syntax for Omega.  Enjoy!

(th~deftheory landau1
              (uses landau0)
	      (help "Definitions, constants, axioms and problems generated from
Jutting's Automath translation of Landau's analysis book.

Landau Chapter 1"))

(th~defconstant _1 (in landau1)
 (type I)
 (help "Constant symbol for the number 1 as used in Grundlagen"))

(th~defconstant _SUC (in landau1)
 (type (I I))
 (help "Constant symbol for successor function on nats as used in Grundlagen"))

(th~defdef _N_IN (in landau1)
 (definition (_ESTI =))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _N_SOME (in landau1)
 (definition (LAM (x0 (O I)) (EXISTS (LAM (x1 I) (x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _N_ALL (in landau1)
 (definition (LAM (x0 (O I)) (FORALL (LAM (x1 I) (x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _N_ONE (in landau1)
 (definition (_E_ONE =))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AX2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (= (_SUC x0) (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AX3 (in landau1)
 (definition (FORALL (LAM (x0 I) (NOT (= (_SUC x0) _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AX4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= (_SUC x0) (_SUC x1)) (= x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COND1 (in landau1)
 (definition (_N_IN _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COND2 (in landau1)
 (definition (LAM (x0 (O I)) (_N_ALL (LAM (x1 I) (IMPLIES (_N_IN x1 x0) (_N_IN (_SUC x1) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AX5 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_COND1 x0) (IMPLIES (_COND2 x0) (FORALL (LAM (x1 I) (_N_IN x1 x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _I1_T1 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (x0 _1) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (x0 (_SUC x1))))) (FORALL (LAM (x2 I) (_COND1 (_SETOF = x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _I1_T2 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (x0 _1) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (x0 (_SUC x1))))) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_N_IN x3 (_SETOF = x0)) (x0 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _I1_T3 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (x0 _1) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (x0 (_SUC x1))))) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_N_IN x3 (_SETOF = x0)) (_N_IN (_SUC x3) (_SETOF = x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _I1_T4 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (x0 _1) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (x0 (_SUC x1))))) (FORALL (LAM (x2 I) (_N_IN x2 (_SETOF = x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INDUCTION (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (x0 _1) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (x0 (_SUC x1))))) (FORALL (LAM (x2 I) (IMPLIES (= x2 x2) (x0 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _21_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (NOT (= x0 x1)) (IMPLIES (= (_SUC x0) (_SUC x1)) (= x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (NOT (= x0 x1)) (NOT (= (_SUC x0) (_SUC x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _22_PROP1 (in landau1)
 (definition (LAM (x0 I) (NOT (= (_SUC x0) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _22_T1 (in landau1)
 (definition (_22_PROP1 _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _22_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_22_PROP1 x0) (_22_PROP1 (_SUC x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ2 (in landau1)
 (definition (FORALL (LAM (x0 I) (NOT (= (_SUC x0) x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _23_PROP1 (in landau1)
 (definition (LAM (x0 I) (OR (= x0 _1) (_N_SOME (LAM (x1 I) (= x0 (_SUC x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _23_T1 (in landau1)
 (definition (_23_PROP1 _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _23_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (_N_SOME (LAM (x1 I) (= (_SUC x0) (_SUC x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _23_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (_23_PROP1 (_SUC x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _23_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (= x0 x0) (_23_PROP1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ3 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (_N_SOME (LAM (x1 I) (= x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _23_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 (_SUC x1)) (IMPLIES (= x0 (_SUC x2)) (= x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _23_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (_AMONE = (LAM (x1 I) (= x0 (_SUC x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ3A (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (_N_ONE (LAM (x1 I) (= x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 (I I)) (_N_ALL (LAM (x2 I) (= (x1 (_SUC x2)) (_SUC (x1 x2))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_PROP2 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 (I I)) (AND (= (x1 _1) (_SUC x0)) (_24_PROP1 x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROP3 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 (I I)) (LAM (x2 (I I)) (LAM (x3 I) (= (x1 x3) (x2 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (= (x1 _1) (_SUC x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (= (x2 _1) (_SUC x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (_PROP3 x0 x1 x2 _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_PROP3 x0 x1 x2 x3) (= (_SUC (x1 x3)) (_SUC (x2 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_PROP3 x0 x1 x2 x3) (_24_PROP1 x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_PROP3 x0 x1 x2 x3) (_24_PROP1 x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T7 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_PROP3 x0 x1 x2 x3) (= (x1 (_SUC x3)) (_SUC (x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T8 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_PROP3 x0 x1 x2 x3) (= (x2 (_SUC x3)) (_SUC (x2 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T9 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_PROP3 x0 x1 x2 x3) (_PROP3 x0 x1 x2 (_SUC x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T10 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (= x3 x3) (_PROP3 x0 x1 x2 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T11 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_24_PROP2 x0 x1) (IMPLIES (_24_PROP2 x0 x2) (= x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AA (in landau1)
 (definition (FORALL (LAM (x0 I) (_AMONE = (_24_PROP2 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROP4 (in landau1)
 (definition (LAM (x0 I) (EXISTS (LAM (x1 (I I)) (AND (= x1 x1) (_24_PROP2 x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T12 (in landau1)
 (definition (_24_PROP1 _1 _SUC))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T13 (in landau1)
 (definition (_24_PROP2 _1 _SUC))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T14 (in landau1)
 (definition (_PROP4 _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T15 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (FORALL (LAM (x2 I) TRUE)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T16 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (= (x1 _1) (_SUC x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T17 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (= (_SUC (x1 _1)) (_SUC (_SUC x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T18 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (FORALL (LAM (x2 I) (_24_PROP1 x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T19 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (FORALL (LAM (x2 I) (= (x1 (_SUC x2)) (_SUC (x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T20 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (FORALL (LAM (x2 I) (= (x1 (_SUC x2)) (_SUC (x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T21 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (FORALL (LAM (x2 I) (= (_SUC (x1 (_SUC x2))) (_SUC (_SUC (x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T22 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (_24_PROP1 (_SUC x0) (LAM (x2 I) (_SUC (x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T23 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (_24_PROP2 (_SUC x0) (LAM (x2 I) (_SUC (x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T24 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_24_PROP2 x0 x1) (_PROP4 (_SUC x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T25 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_PROP4 x0) (_PROP4 (_SUC x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _BB (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (= x0 x0) (_PROP4 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4 (in landau1)
 (definition (FORALL (LAM (x0 I) (_E_ONE = (LAM (x1 (I I)) (AND (= (x1 _1) (_SUC x0)) (_N_ALL (LAM (x2 I) (= (x1 (_SUC x2)) (_SUC (x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PLUS (in landau1)
 (definition (LAM (x0 I) (THAT (LAM (x1 (I I)) (AND (= x1 x1) (_24_PROP2 x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PLUS_DEC (in landau1)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PL (in landau1)
 (definition _PLUS)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PL_DEC (in landau1)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T26 (in landau1)
 (definition (FORALL (LAM (x0 I) (_24_PROP2 x0 (_PLUS x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4A (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_PL x0 _1) (_SUC x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T27 (in landau1)
 (definition (FORALL (LAM (x0 I) (_24_PROP1 x0 (_PLUS x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL x0 (_SUC x1)) (_SUC (_PL x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T28 (in landau1)
 (definition (= (_PLUS _1) _SUC))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4C (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_PL _1 x0) (_SUC x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _24_T29 (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_PLUS (_SUC x0)) (LAM (x1 I) (_SUC (_PLUS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL (_SUC x0) x1) (_SUC (_PL x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4E (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_SUC x0) (_PL x0 _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4F (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_SUC (_PL x0 x1)) (_PL x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4G (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_SUC x0) (_PL _1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ4H (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_SUC (_PL x0 x1)) (_PL (_SUC x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISPL1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_PL x0 x2) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISPL2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISPL12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (= (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _25_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) (= (_PL (_PL x0 x1) x2) (_PL x0 (_PL x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _25_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_25_PROP1 x0 x1 _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _25_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_25_PROP1 x0 x1 x2) (= (_SUC (_PL (_PL x0 x1) x2)) (_SUC (_PL x0 (_PL x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _25_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_25_PROP1 x0 x1 x2) (_25_PROP1 x0 x1 (_SUC x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_PL (_PL x0 x1) x2) (_PL x0 (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSPL1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_PL (_PL x0 x1) x2) (_PL x0 (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSPL2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_PL x0 (_PL x1 x2)) (_PL (_PL x0 x1) x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (= (_PL x0 x1) (_PL x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL x1 _1) (_SUC x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL _1 x1) (_SUC x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_26_PROP1 _1 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_26_PROP1 x0 x1) (= (_SUC (_PL x0 x1)) (_PL x1 (_SUC x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_26_PROP1 x0 x1) (= (_PL (_SUC x0) x1) (_SUC (_PL x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_26_PROP1 x0 x1) (_26_PROP1 (_SUC x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL x0 x1) (_PL x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMPL (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL x0 x1) (_PL x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T7 (in landau1)
 (definition (FORALL (LAM (x0 I) (_26_PROP1 x0 _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _26_T8 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_26_PROP1 x0 x1) (_26_PROP1 x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL x0 x1) (_PL x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _27_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (NOT (= x1 (_PL x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _27_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (NOT (= _1 (_SUC x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _27_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (_27_PROP1 x0 _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _27_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_27_PROP1 x0 x1) (NOT (= (_SUC x1) (_SUC (_PL x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _27_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_27_PROP1 x0 x1) (_27_PROP1 x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ7 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (NOT (= x1 (_PL x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _28_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) (NOT (= (_PL x0 x1) (_PL x0 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _28_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (NOT (= x1 x2)) (NOT (= (_SUC x1) (_SUC x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _28_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (NOT (= x1 x2)) (_28_PROP1 _1 x1 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _28_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (NOT (= x1 x2)) (IMPLIES (_28_PROP1 x0 x1 x2) (NOT (= (_SUC (_PL x0 x1)) (_SUC (_PL x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _28_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (NOT (= x1 x2)) (IMPLIES (_28_PROP1 x0 x1 x2) (_28_PROP1 (_SUC x0) x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ8 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (NOT (= x1 x2)) (NOT (= (_PL x0 x1) (_PL x0 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ8A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_PL x0 x1) (_PL x0 x2)) (= x1 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DIFFPROP (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) (= x0 (_PL x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _28_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (_DIFFPROP x0 x1 x3) (= x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ8B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_AMONE = (LAM (x2 I) (= x0 (_PL x1 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_I (in landau1)
 (definition =)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _II (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (_N_SOME (_DIFFPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _III (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (_N_SOME (_DIFFPROP x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_I x0 x1) (FORALL (LAM (x2 I) (= (_PL x2 x0) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_I x0 x1) (FORALL (LAM (x2 I) (NOT (= x0 (_PL x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_I x0 x1) (NOT (_II x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_I x0 x1) (NOT (_II x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_I x0 x1) (NOT (_III x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_III x0 x1) (NOT (_29_I x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T6A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_II x0 x1) (IMPLIES (_III x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x1 x0 x3) (= x0 (_PL (_PL x3 x2) x0)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T7 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_II x0 x1) (IMPLIES (_III x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x1 x0 x3) FALSE)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T8 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_II x0 x1) (IMPLIES (_III x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) FALSE))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T9 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_II x0 x1) (IMPLIES (_III x0 x1) FALSE)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T10 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_II x0 x1) (NOT (_III x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T11 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_II x0 x1) (NOT (_III x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (AND (IMPLIES (_29_I x0 x1) (NOT (_II x0 x1))) (AND (IMPLIES (_II x0 x1) (NOT (_III x0 x1))) (IMPLIES (_III x0 x1) (NOT (_29_I x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (OR (_29_I x0 x1) (OR (_II x0 x1) (_III x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T12 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (= x0 _1) (_29_PROP1 x0 _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T13 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_SUC x1)) (= x0 (_PL _1 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T14 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_SUC x1)) (_II x0 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T15 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_SUC x1)) (_II x0 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T16 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_SUC x1)) (_29_PROP1 x0 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T16A (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (_29_PROP1 x0 _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T17 (in landau1)
 (definition (FORALL (LAM (x0 I) (_29_PROP1 x0 _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T18 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_29_I x0 x1) (= (_SUC x1) (_PL x0 _1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T19 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_29_I x0 x1) (_III x0 (_SUC x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T20 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_29_I x0 x1) (_29_PROP1 x0 (_SUC x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T21 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (= x2 _1) (= x0 (_SUC x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T22 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (= x2 _1) (_29_PROP1 x0 (_SUC x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T23 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (NOT (= x2 _1)) (FORALL (LAM (x3 I) (IMPLIES (= x2 (_SUC x3)) (= x2 (_PL _1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T24 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (NOT (= x2 _1)) (FORALL (LAM (x3 I) (IMPLIES (= x2 (_SUC x3)) (= x0 (_PL (_SUC x1) x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T25 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (NOT (= x2 _1)) (FORALL (LAM (x3 I) (IMPLIES (= x2 (_SUC x3)) (_II x0 (_SUC x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T26 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (NOT (= x2 _1)) (_II x0 (_SUC x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T27 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (IMPLIES (NOT (= x2 _1)) (_29_PROP1 x0 (_SUC x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T28 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x0 x1 x2) (_29_PROP1 x0 (_SUC x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T28A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_II x0 x1) (_29_PROP1 x0 (_SUC x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T29 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_III x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x1 x0 x2) (= (_SUC x1) (_PL x0 (_SUC x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T30 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_III x0 x1) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x1 x0 x2) (_III x0 (_SUC x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T31 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_III x0 x1) (_III x0 (_SUC x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T32 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (IMPLIES (_III x0 x1) (_29_PROP1 x0 (_SUC x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _29_T33 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_29_PROP1 x0 x1) (_29_PROP1 x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (OR (_29_I x0 x1) (OR (_II x0 x1) (_III x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ9 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (AND (OR (= x0 x1) (OR (_N_SOME (LAM (x2 I) (= x0 (_PL x1 x2)))) (_N_SOME (LAM (x3 I) (= x1 (_PL x0 x3)))))) (AND (IMPLIES (= x0 x1) (NOT (_N_SOME (LAM (x4 I) (= x0 (_PL x1 x4)))))) (AND (IMPLIES (_N_SOME (LAM (x5 I) (= x0 (_PL x1 x5)))) (NOT (_N_SOME (LAM (x6 I) (= x1 (_PL x0 x6)))))) (IMPLIES (_N_SOME (LAM (x7 I) (= x1 (_PL x0 x7)))) (NOT (= x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ9A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (OR (= x0 x1) (OR (_N_SOME (_DIFFPROP x0 x1)) (_N_SOME (_DIFFPROP x1 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ9B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (AND (IMPLIES (= x0 x1) (NOT (_N_SOME (_DIFFPROP x0 x1)))) (AND (IMPLIES (_N_SOME (_DIFFPROP x0 x1)) (NOT (_N_SOME (_DIFFPROP x1 x0)))) (IMPLIES (_N_SOME (_DIFFPROP x1 x0)) (NOT (= x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MORE (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (_N_SOME (_DIFFPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESS (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (_N_SOME (_DIFFPROP x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (AND (OR (= x0 x1) (OR (_MORE x0 x1) (_LESS x0 x1))) (AND (IMPLIES (= x0 x1) (NOT (_MORE x0 x1))) (AND (IMPLIES (_MORE x0 x1) (NOT (_LESS x0 x1))) (IMPLIES (_LESS x0 x1) (NOT (= x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (OR (= x0 x1) (OR (_MORE x0 x1) (_LESS x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (AND (IMPLIES (= x0 x1) (NOT (_MORE x0 x1))) (AND (IMPLIES (_MORE x0 x1) (NOT (_LESS x0 x1))) (IMPLIES (_LESS x0 x1) (NOT (= x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ11 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (_LESS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x0 x1) (_MORE x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREIS (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (OR (_MORE x0 x1) (= x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSIS (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (OR (_LESS x0 x1) (= x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ13 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MOREIS x0 x1) (_LESSIS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ14 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x0 x1) (_MOREIS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMORE1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_MORE x0 x2) (_MORE x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMORE2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_MORE x2 x0) (_MORE x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISLESS1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_LESS x0 x2) (_LESS x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISLESS2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_LESS x2 x0) (_LESS x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMOREIS1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_MOREIS x0 x2) (_MOREIS x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMOREIS2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_MOREIS x2 x0) (_MOREIS x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISLESSIS1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_LESSIS x0 x2) (_LESSIS x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISLESSIS2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (IMPLIES (_LESSIS x2 x0) (_LESSIS x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREISI2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_MOREIS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSISI2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_LESSIS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MOREISI1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (_MOREIS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSISI1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x0 x1) (_LESSIS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMORE12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (IMPLIES (_MORE x0 x2) (_MORE x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISLESS12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (IMPLIES (_LESS x0 x2) (_LESS x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMOREIS12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (IMPLIES (_MOREIS x0 x2) (_MOREIS x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISLESSIS12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (IMPLIES (_LESSIS x0 x2) (_LESSIS x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MOREIS x0 x1) (NOT (_LESS x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x0 x1) (NOT (_MORE x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10E (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (NOT (_MORE x0 x1)) (_LESSIS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10F (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (NOT (_LESS x0 x1)) (_MOREIS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10G (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (NOT (_LESSIS x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10H (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x0 x1) (NOT (_MOREIS x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10J (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (NOT (_MOREIS x0 x1)) (_LESS x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ10K (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (NOT (_LESSIS x0 x1)) (_MORE x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _315_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x1 x2) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x1 x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_DIFFPROP x2 x1 x4) (= x2 (_PL x0 (_PL x3 x4))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _315_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x1 x2) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x1 x0 x3) (FORALL (LAM (x4 I) (IMPLIES (_DIFFPROP x2 x1 x4) (_LESS x0 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _315_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x1 x2) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x1 x0 x3) (_LESS x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ15 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x1 x2) (_LESS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRLESS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x1 x2) (_LESS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRMORE (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x1 x2) (_MORE x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _315_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x1 x2) (_MORE x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ16A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESS x1 x2) (_LESS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ16B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESSIS x1 x2) (_LESS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ16C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MORE x1 x2) (_MORE x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ16D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MOREIS x1 x2) (_MORE x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _317_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (IMPLIES (= x0 x1) (IMPLIES (= x1 x2) (_LESSIS x0 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _317_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (IMPLIES (= x0 x1) (IMPLIES (_LESS x1 x2) (_LESSIS x0 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _317_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (IMPLIES (= x0 x1) (_LESSIS x0 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _317_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (IMPLIES (_LESS x0 x1) (_LESSIS x0 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ17 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (_LESSIS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _317_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (IMPLIES (_LESS x0 x1) (_LESSIS x0 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _317_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (IMPLIES (= x0 x1) (_LESSIS x0 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _317_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (_LESSIS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRLESSIS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x1 x2) (_LESSIS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRMOREIS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x1 x2) (_MOREIS x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ18 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_MORE (_PL x0 x1) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ18A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_LESS x0 (_PL x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ18B (in landau1)
 (definition (FORALL (LAM (x0 I) (_MORE (_SUC x0) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ18C (in landau1)
 (definition (FORALL (LAM (x0 I) (_LESS x0 (_SUC x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _319_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x0 x1 x3) (= x0 (_PL x3 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _319_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x0 x1 x3) (= (_PL x0 x2) (_PL (_PL x1 x2) x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _319_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x0 x1 x3) (_MORE (_PL x0 x2) (_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (_MORE (_PL x0 x2) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_PL x0 x2) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_PL x0 x2) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDERS1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_PL x0 x2) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (_MORE (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19E (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19F (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDERS2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19G (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19H (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_PL x2 x0) (_PL x3 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19J (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19K (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_PL x2 x0) (_PL x3 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _319_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MORE x0 x1) (_MOREIS (_PL x0 x2) (_PL x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _319_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (= x0 x1) (_MOREIS (_PL x0 x2) (_PL x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19L (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (_MOREIS (_PL x0 x2) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19M (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (_MOREIS (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19N (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (_LESSIS (_PL x0 x2) (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ19O (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (_LESSIS (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _320_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (OR (= x0 x1) (OR (_MORE x0 x1) (_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _320_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (AND (IMPLIES (= (_PL x0 x2) (_PL x1 x2)) (NOT (_MORE (_PL x0 x2) (_PL x1 x2)))) (AND (IMPLIES (_MORE (_PL x0 x2) (_PL x1 x2)) (NOT (_LESS (_PL x0 x2) (_PL x1 x2)))) (IMPLIES (_LESS (_PL x0 x2) (_PL x1 x2)) (NOT (= (_PL x0 x2) (_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ20A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE (_PL x0 x2) (_PL x1 x2)) (_MORE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ20B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_PL x0 x2) (_PL x1 x2)) (= x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ20C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS (_PL x0 x2) (_PL x1 x2)) (_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _320_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_PL x0 x2) (_PL x1 x2)) (= (_PL x2 x0) (_PL x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDERSB (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_PL x0 x2) (_PL x1 x2)) (= x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDERSC (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS (_PL x0 x2) (_PL x1 x2)) (_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ20D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE (_PL x2 x0) (_PL x2 x1)) (_MORE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ20E (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_PL x2 x0) (_PL x2 x1)) (= x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ20F (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS (_PL x2 x0) (_PL x2 x1)) (_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _321_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_PL x0 x2) (_PL x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _321_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_PL x1 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ21 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _321_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ21A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDERSA (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ22A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ22B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MOREIS x2 x3) (_MORE (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ22C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ22D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESSIS x2 x3) (_LESS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _323_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (_MOREIS (_PL x0 x2) (_PL x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _323_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (IMPLIES (_MORE x2 x3) (_MOREIS (_PL x0 x2) (_PL x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _323_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (_MOREIS (_PL x0 x2) (_PL x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _323_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (_MORE x0 x1) (_MOREIS (_PL x0 x2) (_PL x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ23 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (_MOREIS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _323_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (_MORE x0 x1) (_MOREIS (_PL x0 x2) (_PL x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _323_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (_MOREIS (_PL x0 x2) (_PL x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _323_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (_MOREIS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ23A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x2 x3) (_LESSIS (_PL x0 x2) (_PL x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _324_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_SUC x1)) (= x0 (_PL _1 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _324_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (FORALL (LAM (x1 I) (IMPLIES (= x0 (_SUC x1)) (_MORE x0 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _324_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (NOT (= x0 _1)) (_MORE x0 _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ24 (in landau1)
 (definition (FORALL (LAM (x0 I) (_MOREIS x0 _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ24A (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (= x0 x0) (_LESSIS _1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ24B (in landau1)
 (definition (FORALL (LAM (x0 I) (_MORE (_SUC x0) _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ24C (in landau1)
 (definition (FORALL (LAM (x0 I) (_LESS _1 (_SUC x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _325_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x1 x0 x2) (_MOREIS (_PL x0 x2) (_PL x0 _1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _325_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_DIFFPROP x1 x0 x2) (_MOREIS x1 (_PL x0 _1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ25 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x1 x0) (_MOREIS x1 (_PL x0 _1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ25A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x1 x0) (_MOREIS x1 (_SUC x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ25B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x1 x0) (_LESSIS (_PL x1 _1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ25C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x1 x0) (_LESSIS (_SUC x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _326_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x1 (_PL x0 _1)) (IMPLIES (_MORE x1 x0) (_MOREIS x1 (_PL x0 _1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _326_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x1 (_PL x0 _1)) (NOT (_MORE x1 x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ26 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x1 (_PL x0 _1)) (_LESSIS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ26A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESS x1 (_SUC x0)) (_LESSIS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ26B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE (_PL x1 _1) x0) (_MOREIS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ26C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE (_SUC x1) x0) (_MOREIS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LBPROP (in landau1)
 (definition (LAM (x0 (O I)) (LAM (x1 I) (LAM (x2 I) (IMPLIES (x0 x2) (_LESSIS x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LB (in landau1)
 (definition (LAM (x0 (O I)) (LAM (x1 I) (_N_ALL (_LBPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MIN (in landau1)
 (definition (LAM (x0 (O I)) (LAM (x1 I) (AND (_LB x0 x1) (x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T1 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_LBPROP x0 _1 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T2 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (_LB x0 _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T3 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_LB x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (_MORE (_PL x2 _1) x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T4 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_LB x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (NOT (_LESSIS (_PL x2 _1) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T5 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_LB x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (NOT (_LBPROP x0 (_PL x2 _1) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T6 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_LB x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (NOT (_LB x0 (_PL x2 _1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T7 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_LB x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) FALSE))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T8 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_LB x0 x1)))) FALSE)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T9 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (NOT (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1))))))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (NOT (AND (_LB x0 x2) (NOT (_LB x0 (_PL x2 _1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T10 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (NOT (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1))))))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (_LB x0 (_PL x2 _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T11 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (NOT (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1))))))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (_LB x0 (_SUC x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T12 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (IMPLIES (FORALL (LAM (x1 I) (NOT (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1))))))) (FORALL (LAM (x2 I) (IMPLIES (= x2 x2) (_LB x0 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T13 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (_N_SOME (LAM (x1 I) (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T14 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (_LB x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T15 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (NOT (_LB x0 (_PL x1 _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T16 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (IMPLIES (NOT (x0 x1)) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (_LESSIS x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T17 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (IMPLIES (NOT (x0 x1)) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (NOT (= x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T18 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (IMPLIES (NOT (x0 x1)) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (_LESS x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T19 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (IMPLIES (NOT (x0 x1)) (FORALL (LAM (x2 I) (IMPLIES (x0 x2) (_LESSIS (_PL x1 _1) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T20 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (IMPLIES (NOT (x0 x1)) (_LB x0 (_PL x1 _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T21 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (IMPLIES (NOT (x0 x1)) FALSE))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T22 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T23 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (AND (_LB x0 x1) (NOT (_LB x0 (_PL x1 _1)))) (_MIN x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ27 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (_N_SOME (_MIN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T24 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (= x2 x2) (_LBPROP x0 _1 x2))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T25 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (_LB x0 _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T26 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (NOT (_MIN x0 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T27 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (NOT (x0 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T28 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (FORALL (LAM (x3 I) (IMPLIES (x0 x3) (NOT (= x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T29 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (FORALL (LAM (x3 I) (IMPLIES (x0 x3) (_LESSIS x2 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T30 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (FORALL (LAM (x3 I) (IMPLIES (x0 x3) (_LESS x2 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T31 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (FORALL (LAM (x3 I) (IMPLIES (x0 x3) (_LESSIS (_SUC x2) x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T32 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (FORALL (LAM (x3 I) (IMPLIES (= x3 x3) (_LBPROP x0 (_SUC x2) x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T33 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (_LB x0 x2) (_LB x0 (_SUC x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T34 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (FORALL (LAM (x1 I) (NOT (_MIN x0 x1)))) (FORALL (LAM (x2 I) (IMPLIES (= x2 x2) (_LB x0 x2))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T35 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (NOT (_LESSIS (_SUC x1) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_T36 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (NOT (_LBPROP x0 (_SUC x1) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T37 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (NOT (_LB x0 (_SUC x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T38 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (FORALL (LAM (x1 I) (IMPLIES (x0 x1) (_N_SOME (_MIN x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _327_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (_N_SOME (_MIN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T39 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (_LB x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T40 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (_LB x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T41 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T42 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T43 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (_LBPROP x0 x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T44 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (_LBPROP x0 x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T45 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (_LESSIS x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T46 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (_LESSIS x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T47 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MIN x0 x1) (IMPLIES (_MIN x0 x2) (= x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T48 (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (_AMONE = (_MIN x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ27A (in landau1)
 (definition (FORALL (LAM (x0 (O I)) (IMPLIES (_N_SOME x0) (_N_ONE (_MIN x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 (I I)) (_N_ALL (LAM (x2 I) (= (x1 (_SUC x2)) (_PL (x1 x2) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_PROP2 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 (I I)) (AND (= (x1 _1) x0) (_428_PROP1 x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_PROP3 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 (I I)) (LAM (x2 (I I)) (LAM (x3 I) (= (x1 x3) (x2 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (= (x1 _1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (= (x2 _1) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (_428_PROP3 x0 x1 x2 _1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_428_PROP3 x0 x1 x2 x3) (= (_PL (x1 x3) x0) (_PL (x2 x3) x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_428_PROP3 x0 x1 x2 x3) (_428_PROP1 x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_428_PROP3 x0 x1 x2 x3) (_428_PROP1 x0 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T7 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_428_PROP3 x0 x1 x2 x3) (= (x1 (_SUC x3)) (_PL (x1 x3) x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T8 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_428_PROP3 x0 x1 x2 x3) (= (x2 (_SUC x3)) (_PL (x2 x3) x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T9 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (_428_PROP3 x0 x1 x2 x3) (_428_PROP3 x0 x1 x2 (_SUC x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T10 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (FORALL (LAM (x3 I) (IMPLIES (= x3 x3) (_428_PROP3 x0 x1 x2 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T11 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 (I I)) (FORALL (LAM (x2 (I I)) (IMPLIES (_428_PROP2 x0 x1) (IMPLIES (_428_PROP2 x0 x2) (= x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _A1 (in landau1)
 (definition (FORALL (LAM (x0 I) (_AMONE = (_428_PROP2 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_PROP4 (in landau1)
 (definition (LAM (x0 I) (EXISTS (LAM (x1 (I I)) (AND (= x1 x1) (_428_PROP2 x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T12 (in landau1)
 (definition (_428_PROP1 _1 (LAM (x0 I) x0)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T13 (in landau1)
 (definition (_428_PROP2 _1 (LAM (x0 I) x0)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T14 (in landau1)
 (definition (_428_PROP4 _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T15 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (= (x1 _1) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T16 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (= (_PL (x1 _1) _1) (_SUC x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T17 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (FORALL (LAM (x2 I) (_428_PROP1 x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T18 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (FORALL (LAM (x2 I) (= (x1 (_SUC x2)) (_PL (x1 x2) x0)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T19 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (FORALL (LAM (x2 I) (= (_PL (x1 (_SUC x2)) (_SUC x2)) (_PL (x1 x2) (_PL x0 (_SUC x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T20 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (FORALL (LAM (x2 I) (= (_PL x0 (_SUC x2)) (_PL x2 (_SUC x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T21 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (FORALL (LAM (x2 I) (= (_PL (x1 (_SUC x2)) (_SUC x2)) (_PL (_PL (x1 x2) x2) (_SUC x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T22 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (_428_PROP1 (_SUC x0) (LAM (x2 I) (_PL (x1 x2) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T23 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (_428_PROP2 (_SUC x0) (LAM (x2 I) (_PL (x1 x2) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T24 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (FORALL (LAM (x1 (I I)) (IMPLIES (_428_PROP2 x0 x1) (_428_PROP4 (_SUC x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T25 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_428_PROP4 x0) (_428_PROP4 (_SUC x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _B1 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (= x0 x0) (_428_PROP4 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28 (in landau1)
 (definition (FORALL (LAM (x0 I) (_E_ONE = (LAM (x1 (I I)) (AND (= (x1 _1) x0) (_N_ALL (LAM (x2 I) (= (x1 (_SUC x2)) (_PL (x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TIMES (in landau1)
 (definition (LAM (x0 I) (THAT (LAM (x1 (I I)) (AND (= x1 x1) (_428_PROP2 x0 x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TIMES_DEC (in landau1)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T26 (in landau1)
 (definition (FORALL (LAM (x0 I) (_428_PROP2 x0 (_TIMES x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28A (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_TIMES x0 _1) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T27 (in landau1)
 (definition (FORALL (LAM (x0 I) (_428_PROP1 x0 (_TIMES x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES x0 (_SUC x1)) (_PL (_TIMES x0 x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T28 (in landau1)
 (definition (= (_TIMES _1) (LAM (x0 I) x0)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28C (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_TIMES _1 x0) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _428_T29 (in landau1)
 (definition (FORALL (LAM (x0 I) (= (_TIMES (_SUC x0)) (LAM (x1 I) (_PL (_TIMES x0 x1) x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES (_SUC x0) x1) (_PL (_TIMES x0 x1) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28E (in landau1)
 (definition (FORALL (LAM (x0 I) (= x0 (_TIMES x0 _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28F (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL (_TIMES x0 x1) x0) (_TIMES x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28G (in landau1)
 (definition (FORALL (LAM (x0 I) (= x0 (_TIMES _1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ28H (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_PL (_TIMES x0 x1) x1) (_TIMES (_SUC x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _N_ISTS1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _N_ISTS2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_TIMES x2 x0) (_TIMES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISTS12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (= (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (= (_TIMES x0 x1) (_TIMES x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES x1 _1) x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES _1 x1) x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x1 x1) (_429_PROP1 _1 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_429_PROP1 x0 x1) (= (_PL (_TIMES x0 x1) x1) (_TIMES x1 (_SUC x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_429_PROP1 x0 x1) (= (_TIMES (_SUC x0) x1) (_PL (_TIMES x0 x1) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_429_PROP1 x0 x1) (_429_PROP1 (_SUC x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ29 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES x0 x1) (_TIMES x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMTS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES x0 x1) (_TIMES x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T7 (in landau1)
 (definition (FORALL (LAM (x0 I) (_429_PROP1 x0 _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_T8 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_429_PROP1 x0 x1) (_429_PROP1 x0 (_SUC x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _429_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (= (_TIMES x0 x1) (_TIMES x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _430_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) (= (_TIMES x0 (_PL x1 x2)) (_PL (_TIMES x0 x1) (_TIMES x0 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _430_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_430_PROP1 x0 x1 _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _430_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_430_PROP1 x0 x1 x2) (= (_TIMES x0 (_PL x1 (_SUC x2))) (_PL (_PL (_TIMES x0 x1) (_TIMES x0 x2)) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _430_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_430_PROP1 x0 x1 x2) (_430_PROP1 x0 x1 (_SUC x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ30 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES x0 (_PL x1 x2)) (_PL (_TIMES x0 x1) (_TIMES x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTTP1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_PL x0 x1) x2) (_PL (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTTP2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES x0 (_PL x1 x2)) (_PL (_TIMES x0 x1) (_TIMES x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTPT1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_PL (_TIMES x0 x2) (_TIMES x1 x2)) (_TIMES (_PL x0 x1) x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISTPT2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_PL (_TIMES x0 x1) (_TIMES x0 x2)) (_TIMES x0 (_PL x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _431_PROP1 (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) (= (_TIMES (_TIMES x0 x1) x2) (_TIMES x0 (_TIMES x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _431_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (_431_PROP1 x0 x1 _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _431_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_431_PROP1 x0 x1 x2) (_431_PROP1 x0 x1 (_SUC x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ31 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_TIMES x0 x1) x2) (_TIMES x0 (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSTS1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES (_TIMES x0 x1) x2) (_TIMES x0 (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ASSTS2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (= (_TIMES x0 (_TIMES x1 x2)) (_TIMES (_TIMES x0 x1) x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _432_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x0 x1 x3) (= (_TIMES x0 x2) (_PL (_TIMES x1 x2) (_TIMES x3 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _432_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (FORALL (LAM (x3 I) (IMPLIES (_DIFFPROP x0 x1 x3) (_MORE (_TIMES x0 x2) (_TIMES x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (_MORE (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _432_ANDERS1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (_MORE (_TIMES x2 x0) (_TIMES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32E (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x0 x1) (= (_TIMES x2 x0) (_TIMES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32F (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_TIMES x2 x0) (_TIMES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _432_ANDERS2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS x0 x1) (_LESS (_TIMES x2 x0) (_TIMES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32G (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32H (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_TIMES x2 x0) (_TIMES x3 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32J (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32K (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_TIMES x2 x0) (_TIMES x3 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _432_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MORE x0 x1) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _432_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (= x0 x1) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32L (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32M (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MOREIS x0 x1) (_MOREIS (_TIMES x2 x0) (_TIMES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32N (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (_LESSIS (_TIMES x0 x2) (_TIMES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ32O (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x0 x1) (_LESSIS (_TIMES x2 x0) (_TIMES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _433_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (OR (= x0 x1) (OR (_MORE x0 x1) (_LESS x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _433_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (AND (IMPLIES (= (_TIMES x0 x2) (_TIMES x1 x2)) (NOT (_MORE (_TIMES x0 x2) (_TIMES x1 x2)))) (AND (IMPLIES (_MORE (_TIMES x0 x2) (_TIMES x1 x2)) (NOT (_LESS (_TIMES x0 x2) (_TIMES x1 x2)))) (IMPLIES (_LESS (_TIMES x0 x2) (_TIMES x1 x2)) (NOT (= (_TIMES x0 x2) (_TIMES x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ33A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE (_TIMES x0 x2) (_TIMES x1 x2)) (_MORE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ33B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= (_TIMES x0 x2) (_TIMES x1 x2)) (= x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ33C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS (_TIMES x0 x2) (_TIMES x1 x2)) (_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _433_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_LESS (_TIMES x0 x2) (_TIMES x1 x2)) (_LESS x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _434_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_TIMES x0 x2) (_TIMES x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _434_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_TIMES x1 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ34 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _434_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ34A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _434_ANDERSA (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ35A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MORE x2 x3) (_MORE (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ35B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x1) (IMPLIES (_MOREIS x2 x3) (_MORE (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ35C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESS x2 x3) (_LESS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ35D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESS x0 x1) (IMPLIES (_LESSIS x2 x3) (_LESS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _436_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _436_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (IMPLIES (_MORE x2 x3) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _436_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _436_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (_MORE x0 x1) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ36 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _436_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (_MORE x0 x1) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _436_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (IMPLIES (= x0 x1) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _436_ANDERS (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MOREIS x0 x1) (IMPLIES (_MOREIS x2 x3) (_MOREIS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SATZ36A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_LESSIS x0 x1) (IMPLIES (_LESSIS x2 x3) (_LESSIS (_TIMES x0 x2) (_TIMES x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MN_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (_N_ONE (_DIFFPROP x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MN (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (THAT (_DIFFPROP x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MN_DEC (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x2 x3) (IMPLIES (_MORE x0 x2) (= (_MN x0 x2) (_MN x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH1A (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (= x0 (_PL x1 (_MN x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH1B (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (= (_PL x1 (_MN x0 x1)) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH1C (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (= x0 (_PL (_MN x0 x1) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH1D (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_MORE x0 x1) (= (_PL (_MN x0 x1) x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH1E (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_MORE x0 x1) (IMPLIES (= (_PL x1 x2) x0) (= x2 (_MN x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _MN_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x2) (IMPLIES (_MORE x1 x3) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (= (_PL x3 (_MN x0 x2)) x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISMN12 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (_MORE x0 x2) (IMPLIES (_MORE x1 x3) (IMPLIES (= x0 x1) (IMPLIES (= x2 x3) (= (_MN x0 x2) (_MN x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1TO (in landau1)
 (definition (LAM (x0 I) (_OT = (LAM (x1 I) (_LESSIS x1 x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1TO_DEC (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (= x0 x0) (PER (_1TO x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1TOP (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_1TO x0 x1 x1) (_LESSIS x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISOUTNI (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x2 x0) (IMPLIES (= x1 x2) (_1TO x0 (_OUT = (LAM (x3 I) (_LESSIS x3 x0)) x1) (_OUT = (LAM (x4 I) (_LESSIS x4 x0)) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISOUTNE (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_LESSIS x2 x0) (IMPLIES (_1TO x0 (_OUT = (LAM (x3 I) (_LESSIS x3 x0)) x1) (_OUT = (LAM (x4 I) (_LESSIS x4 x0)) x2)) (= x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINNI (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_1TO x0 x1 x1) (FORALL (LAM (x2 I) (IMPLIES (_1TO x0 x2 x2) (IMPLIES (_1TO x0 x1 x2) (= x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINNE (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_1TO x0 x1 x1) (FORALL (LAM (x2 I) (IMPLIES (_1TO x0 x2 x2) (IMPLIES (= x1 x2) (_1TO x0 x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISOUTINN (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_1TO x0 x1 x1) (_1TO x0 x1 (_OUT = (LAM (x2 I) (_LESSIS x2 x0)) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINOUTN (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (= x1 (_OUT = (LAM (x2 I) (_LESSIS x2 x0)) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1O (in landau1)
 (definition (_OUT = (LAM (x0 I) (_LESSIS x0 _1)) _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1O_DEC (in landau1)
 (definition (_1TO _1 _1O _1O))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _U0 (in landau1)
 (definition (LAM (x0 I) x0))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _U0_DEC (in landau1)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SINGLET_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _1 x0 x0) (_LESSIS (_U0 x0) _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SINGLET_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _1 x0 x0) (= (_U0 x0) _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SINGLET_TH1 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _1 x0 x0) (_1TO _1 x0 _1O)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2 (in landau1)
 (definition (_PL _1 _1))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_DEC (in landau1)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_LESSIS x0 _2) (IMPLIES (NOT (= x0 _2)) (_LESSIS x0 _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_LESSIS x0 _2) (IMPLIES (NOT (= x0 _2)) (= x0 _1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_TH1 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_LESSIS x0 _2) (OR (= x0 _1) (= x0 _2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_TH2 (in landau1)
 (definition (NOT (= _2 _1)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _2 x0 x0) (_LESSIS x0 _2)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _2 x0 x0) (IMPLIES (= x0 _1) (_1TO _2 (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) x0) (_OUT = (LAM (x2 I) (_LESSIS x2 _2)) _1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _2 x0 x0) (IMPLIES (= x0 _1) (_1TO _2 x0 (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) _1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _2 x0 x0) (IMPLIES (= x0 _2) (_1TO _2 (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) x0) (_OUT = (LAM (x2 I) (_LESSIS x2 _2)) _2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T7 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _2 x0 x0) (IMPLIES (= x0 _2) (_1TO _2 x0 (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) _2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_TH3 (in landau1)
 (definition (FORALL (LAM (x0 I) (IMPLIES (_1TO _2 x0 x0) (OR (_1TO _2 x0 (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) _1)) (_1TO _2 x0 (_OUT = (LAM (x2 I) (_LESSIS x2 _2)) _2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T9 (in landau1)
 (definition (IMPLIES (_1TO _2 (_OUT = (LAM (x0 I) (_LESSIS x0 _2)) _2) (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) _1)) (= (_OUT = (LAM (x2 I) (_LESSIS x2 _2)) _2) (_OUT = (LAM (x3 I) (_LESSIS x3 _2)) _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T10 (in landau1)
 (definition (IMPLIES (_1TO _2 (_OUT = (LAM (x0 I) (_LESSIS x0 _2)) _2) (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) _1)) (= _2 _1)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_TH4 (in landau1)
 (definition (NOT (_1TO _2 (_OUT = (LAM (x0 I) (_LESSIS x0 _2)) _2) (_OUT = (LAM (x1 I) (_LESSIS x1 _2)) _1))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR1TYPE (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (Z I)) (LAM (x2 (Z I)) (FORALL (LAM (x3 I) (FORALL (LAM (x4 I) (IMPLIES (_1TO _2 x3 x4) (x0 (x1 x3) (x2 x4)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR1TYPE_DEC (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (PER (_PAIR1TYPE x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR1 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 Z) (LAM (x2 Z) (LAM (x3 I) (_ITE (_1TO _2 x3 (_OUT = (LAM (x4 I) (_LESSIS x4 _2)) _1)) x0 x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR1_DEC (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (FORALL (LAM (x2 Z) (IMPLIES (x0 x1 x2) (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (_PAIR1TYPE x0 (_PAIR1 x0 x1 x3) (_PAIR1 x0 x2 x4)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIRST1 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (Z I)) (x1 (_OUT = (LAM (x2 I) (_LESSIS x2 _2)) _1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIRST1_DEC (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x2) (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SECOND1 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (Z I)) (x1 (_OUT = (LAM (x2 I) (_LESSIS x2 _2)) _2)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SECOND1_DEC (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x2) (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIRST1IS1 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x0 (_FIRST1 x0 (_PAIR1 x0 x1 x2)) x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIRST1IS2 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x0 x1 (_FIRST1 x0 (_PAIR1 x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SECOND1IS1 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x0 (_SECOND1 x0 (_PAIR1 x0 x1 x2)) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SECOND1IS2 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x0 x2 (_SECOND1 x0 (_PAIR1 x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T11 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (FORALL (LAM (x3 I) (IMPLIES (_1TO _2 x3 x3) (IMPLIES (_1TO _2 x3 (_OUT = (LAM (x4 I) (_LESSIS x4 _2)) _1)) (x0 (x1 x3) (_FIRST1 x0 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T12 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (FORALL (LAM (x3 I) (IMPLIES (_1TO _2 x3 x3) (IMPLIES (_1TO _2 x3 (_OUT = (LAM (x4 I) (_LESSIS x4 _2)) _1)) (x0 (_FIRST1 x0 x2) (x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T13 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (FORALL (LAM (x3 I) (IMPLIES (_1TO _2 x3 x3) (IMPLIES (_1TO _2 x3 (_OUT = (LAM (x4 I) (_LESSIS x4 _2)) _1)) (x0 (x1 x3) (x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T14 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (FORALL (LAM (x3 I) (IMPLIES (_1TO _2 x3 x3) (IMPLIES (_1TO _2 x3 (_OUT = (LAM (x4 I) (_LESSIS x4 _2)) _2)) (x0 (x1 x3) (_SECOND1 x0 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T15 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (FORALL (LAM (x3 I) (IMPLIES (_1TO _2 x3 x3) (IMPLIES (_1TO _2 x3 (_OUT = (LAM (x4 I) (_LESSIS x4 _2)) _2)) (x0 (_SECOND1 x0 x2) (x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T16 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (FORALL (LAM (x3 I) (IMPLIES (_1TO _2 x3 x3) (IMPLIES (_1TO _2 x3 (_OUT = (LAM (x4 I) (_LESSIS x4 _2)) _2)) (x0 (x1 x3) (x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T17 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (FORALL (LAM (x3 I) (IMPLIES (_1TO _2 x3 x3) (x0 (x1 x3) (x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_TH5 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (FORALL (LAM (x2 (Z I)) (IMPLIES (_PAIR1TYPE x0 x2 x2) (IMPLIES (x0 (_FIRST1 x0 x1) (_FIRST1 x0 x2)) (IMPLIES (x0 (_SECOND1 x0 x1) (_SECOND1 x0 x2)) (_PAIR1TYPE x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T18 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (x0 (_FIRST1 x0 (_PAIR1 x0 (_FIRST1 x0 x1) (_SECOND1 x0 x1))) (_FIRST1 x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR_T19 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (x0 (_SECOND1 x0 (_PAIR1 x0 (_FIRST1 x0 x1) (_SECOND1 x0 x1))) (_SECOND1 x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR1IS1 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (_PAIR1TYPE x0 (_PAIR1 x0 (_FIRST1 x0 x1) (_SECOND1 x0 x1)) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PAIR1IS2 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (Z I)) (IMPLIES (_PAIR1TYPE x0 x1 x1) (_PAIR1TYPE x0 x1 (_PAIR1 x0 (_FIRST1 x0 x1) (_SECOND1 x0 x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LESSISI3 (in landau1)
 (definition (FORALL (LAM (x0 I) (_LESSIS x0 x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1OUT (in landau1)
 (definition (LAM (x0 I) (_OUT = (LAM (x1 I) (_LESSIS x1 x0)) _1)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1OUT_DEC (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_1TO x0 (_1OUT x0) (_1OUT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XOUT (in landau1)
 (definition (LAM (x0 I) (_OUT = (LAM (x1 I) (_LESSIS x1 x0)) x0)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _XOUT_DEC (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (_1TO x0 (_XOUT x0) (_XOUT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UI (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) x2))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UI_DEC (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x2 x3) (IMPLIES (_LESSIS x2 x0) (= (_UI x0 x2) (_UI x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (_LESSIS (_UI x0 x1 x2) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (_LESSIS (_UI x0 x1 x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT1TO (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) (_OUT = (LAM (x3 I) (_LESSIS x3 x0)) (_UI x0 x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT1TO_DEC (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x2 x3) (IMPLIES (_LESSIS x2 x0) (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (_1TO x0 (_LEFT1TO x0 x2 x4) (_LEFT1TO x1 x3 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T3 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (FORALL (LAM (x3 I) (IMPLIES (_1TO x1 x3 x3) (IMPLIES (_1TO x0 (_LEFT1TO x0 x1 x2) (_LEFT1TO x0 x1 x3)) (= (_UI x0 x1 x2) (_UI x0 x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THLEFT1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (FORALL (LAM (x3 I) (IMPLIES (_1TO x1 x3 x3) (IMPLIES (_1TO x0 (_LEFT1TO x0 x1 x2) (_LEFT1TO x0 x1 x3)) (_1TO x1 x2 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THLEFT2 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (_LESSIS x1 x0) (_INJECTIVE (_1TO x1) (_1TO x0) (_LEFT1TO x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT_UI (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) x2))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT_UI_DEC (in landau1)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT_T4 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (_LESSIS (_RIGHT_UI x0 x1 x2) x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT_T5 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (_LESSIS (_PL x0 (_RIGHT_UI x0 x1 x2)) (_PL x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT1TO (in landau1)
 (definition (LAM (x0 I) (LAM (x1 I) (LAM (x2 I) (_OUT = (LAM (x3 I) (_LESSIS x3 (_PL x0 x1))) (_PL x0 (_RIGHT_UI x0 x1 x2)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT1TO_DEC (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (IMPLIES (= x0 x1) (FORALL (LAM (x2 I) (FORALL (LAM (x3 I) (IMPLIES (= x2 x3) (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (_1TO (_PL x0 x2) (_RIGHT1TO x0 x2 x4) (_RIGHT1TO x1 x3 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT_T6 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (FORALL (LAM (x3 I) (IMPLIES (_1TO x1 x3 x3) (IMPLIES (_1TO (_PL x0 x1) (_RIGHT1TO x0 x1 x2) (_RIGHT1TO x0 x1 x3)) (= (_PL x0 (_RIGHT_UI x0 x1 x2)) (_PL x0 (_RIGHT_UI x0 x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT_T7 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (FORALL (LAM (x3 I) (IMPLIES (_1TO x1 x3 x3) (IMPLIES (_1TO (_PL x0 x1) (_RIGHT1TO x0 x1 x2) (_RIGHT1TO x0 x1 x3)) (= (_RIGHT_UI x0 x1 x2) (_RIGHT_UI x0 x1 x3))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THRIGHT1 (in landau1)
 (definition (FORALL (LAM (x0 I) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (_1TO x1 x2 x2) (FORALL (LAM (x3 I) (IMPLIES (_1TO x1 x3 x3) (IMPLIES (_1TO (_PL x0 x1) (_RIGHT1TO x0 x1 x2) (_RIGHT1TO x0 x1 x3)) (_1TO x1 x2 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 I) (LAM (x2 I) (LAM (x3 (Z I)) (LAM (x4 I) (x3 (_LEFT1TO x1 x2 x4))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_DEC (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x1 x2) (FORALL (LAM (x3 I) (FORALL (LAM (x4 I) (IMPLIES (= x3 x4) (IMPLIES (_LESSIS x3 x1) (FORALL (LAM (x5 (Z I)) (FORALL (LAM (x6 (Z I)) (IMPLIES (FORALL (LAM (x7 I) (FORALL (LAM (x8 I) (IMPLIES (_1TO x1 x7 x8) (x0 (x5 x7) (x6 x8))))))) (FORALL (LAM (x9 I) (FORALL (LAM (x10 I) (IMPLIES (_1TO x3 x9 x10) (x0 (_LEFT x0 x1 x3 x5 x9) (_LEFT x0 x2 x4 x6 x10))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 I) (LAM (x2 I) (LAM (x3 (Z I)) (LAM (x4 I) (x3 (_RIGHT1TO x1 x2 x4))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _RIGHT_DEC (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x1 x2) (FORALL (LAM (x3 I) (FORALL (LAM (x4 I) (IMPLIES (= x3 x4) (FORALL (LAM (x5 (Z I)) (FORALL (LAM (x6 (Z I)) (IMPLIES (FORALL (LAM (x7 I) (FORALL (LAM (x8 I) (IMPLIES (_1TO (_PL x1 x3) x7 x8) (x0 (x5 x7) (x6 x8))))))) (FORALL (LAM (x9 I) (FORALL (LAM (x10 I) (IMPLIES (_1TO x3 x9 x10) (x0 (_RIGHT x0 x1 x3 x5 x9) (_RIGHT x0 x2 x4 x6 x10)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T4 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x2 x1) (FORALL (LAM (x3 (Z I)) (IMPLIES (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (x0 (x3 x4) (x3 x5))))))) (_LESSIS x2 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T5 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x2 x1) (FORALL (LAM (x3 (Z I)) (IMPLIES (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (x0 (x3 x4) (x3 x5))))))) (_LESSIS x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T6 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x2 x1) (FORALL (LAM (x3 (Z I)) (IMPLIES (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (x0 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 I) (IMPLIES (_1TO x2 x6 x6) (= x6 (_LEFT1TO x1 x2 x6))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T7 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x2 x1) (FORALL (LAM (x3 (Z I)) (IMPLIES (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (x0 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 I) (IMPLIES (_1TO x2 x6 x6) (_1TO x2 x6 (_LEFT1TO x2 x1 (_LEFT1TO x1 x2 x6)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _LEFT_T8 (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x2 x1) (FORALL (LAM (x3 (Z I)) (IMPLIES (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (x0 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 I) (IMPLIES (_1TO x2 x6 x6) (x0 (x3 x6) (_LEFT x0 x1 x2 (_LEFT x0 x2 x1 x3) x6))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THLEFT (in landau1)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 I) (FORALL (LAM (x2 I) (IMPLIES (= x2 x1) (FORALL (LAM (x3 (Z I)) (IMPLIES (FORALL (LAM (x4 I) (FORALL (LAM (x5 I) (IMPLIES (_1TO x2 x4 x5) (x0 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 I) (FORALL (LAM (x7 I) (IMPLIES (_1TO x2 x6 x7) (x0 (x3 x6) (_LEFT x0 x1 x2 (_LEFT x0 x2 x1 x3) x7))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

