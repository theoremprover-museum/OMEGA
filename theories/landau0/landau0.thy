; Chad E Brown; In November 2005 I translated Jutting's Automath version of
; Landau's Grundlagen der Analysis into TPS (without proofs) and then
; from TPS into this post syntax for Omega.  Enjoy!

(th~deftheory landau0
              (uses base typed-set)
	      (help "Definitions, constants, axioms and problems generated from
Jutting's Automath translation of Landau's analysis book.
Mainly Automath Preliminaries"))

(th~defdef TRANSITIVE (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (x0 (O A A)) (FORALL (LAM (x1 A) (FORALL (LAM (x2 A) (FORALL (LAM (x3 A) (IMPLIES (AND (x0 x1 x2) (x0 x2 x3)) (x0 x1 x3))))))))))
 (help "Predicate for transitive relations"))

(th~defdef SYMMETRIC (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (x0 (O A A)) (FORALL (LAM (x1 A) (FORALL (LAM (x2 A) (IMPLIES (x0 x1 x2) (x0 x2 x1))))))))
 (help "Predicate for symmetric relations"))

(th~defdef PER (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (x0 (O A A)) (AND (SYMMETRIC x0) (TRANSITIVE x0))))
 (help "PER -- Partial Equivalence Relation -- Relation which is symmetric and transitive (but not nec. refl.)"))

(th~defdef _MP (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x0 (IMPLIES (IMPLIES x0 x1) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFIMP (in landau0)
 (definition (FORALL (LAM (x0 O) (IMPLIES x0 x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRIMP (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (IMPLIES x0 x1) (IMPLIES (IMPLIES x1 x2) (IMPLIES x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WELI (in landau0)
 (definition (FORALL (LAM (x0 O) (IMPLIES x0 (NOT (NOT x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ET (in landau0)
 (definition (FORALL (LAM (x0 O) (IMPLIES (NOT (NOT x0)) x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CONE (in landau0)
 (definition (FORALL (LAM (x0 O) (IMPLIES TRUE (IMPLIES FALSE x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 x1) (IMPLIES (IMPLIES (NOT x0) x1) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x0) (IMPLIES x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x1) (IMPLIES (IMPLIES x0 x1) (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x0 (IMPLIES (NOT x1) (NOT (IMPLIES x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (IMPLIES x0 x1)) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (IMPLIES x0 x1)) (NOT x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH7 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x1) (IMPLIES (IMPLIES (NOT x0) x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CP (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES (NOT x1) (NOT x0)) (IMPLIES x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OBVIOUSI (in landau0)
 (definition TRUE)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECI1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x0) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECI2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x1) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 (NOT x1)) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x1 (NOT x0)) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMEC (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 (NOT x1)) (IMPLIES x1 (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECE1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 (NOT x1)) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECE2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 (NOT x1)) (IMPLIES x1 (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (IMPLIES x0 (NOT x1)) (IMPLIES (IMPLIES x2 x0) (IMPLIES x2 (NOT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (IMPLIES x0 (NOT x1)) (IMPLIES (IMPLIES x2 x1) (IMPLIES x0 (NOT x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDI (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x0 (IMPLIES x1 (AND x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDE1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (AND x0 x1) x0))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANDE2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (AND x0 x1) x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMAND (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (AND x0 x1) (AND x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x0) (NOT (AND x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x1) (NOT (AND x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (AND x0 x1)) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (AND x0 x1)) (IMPLIES x1 (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND_TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (AND x0 x1)) (NOT (AND x1 x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND_TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 x1) (IMPLIES (IMPLIES x0 x2) (AND x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND_TH7 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 x1) (IMPLIES (IMPLIES x1 x2) (AND x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORI1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x0 (OR x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORI2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x1 (OR x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES (NOT x0) x1) (OR x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES (NOT x1) x0) (OR x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORE2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR x0 x1) (IMPLIES (NOT x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORE1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR x0 x1) (IMPLIES (NOT x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMOR (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR x0 x1) (OR x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x0) (IMPLIES (NOT x1) (NOT (OR x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (OR x0 x1)) (NOT x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR_TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (OR x0 x1)) (NOT x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR_TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (OR x0 (NOT x0)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORAPP (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 x1) (IMPLIES (IMPLIES x0 x2) (IMPLIES (IMPLIES x1 x2) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR_TH7 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 x1) (IMPLIES (IMPLIES x0 x2) (OR x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH8 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 x1) (IMPLIES (IMPLIES x1 x2) (OR x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH9 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (FORALL (LAM (x3 O) (IMPLIES (OR x0 x1) (IMPLIES (IMPLIES x0 x2) (IMPLIES (IMPLIES x1 x3) (OR x2 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH10 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR x0 x1) (IMPLIES (NOT x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH11 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR x0 x1) (IMPLIES (NOT x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH12 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR (NOT x0) x1) (IMPLIES x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH13 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 x1) (OR (NOT x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH14 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR (NOT x0) (NOT x1)) (NOT (AND x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH15 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (AND x0 x1)) (OR (NOT x0) (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH16 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (AND (NOT x0) (NOT x1)) (NOT (OR x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH17 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT (OR x0 x1)) (AND (NOT x0) (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC (in landau0)
 (definition (LAM (x0 O) (LAM (x1 O) (AND (OR x0 x1) (IMPLIES x0 (NOT x1))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORECI (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (OR x0 x1) (IMPLIES (IMPLIES x0 (NOT x1)) (_OREC x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x0 (IMPLIES (NOT x1) (_OREC x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x0) (IMPLIES x1 (_OREC x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORECE1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (OR x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ORECE2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMOREC (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (_OREC x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (IMPLIES x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (IMPLIES x1 (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC_TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (IMPLIES (NOT x0) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC_TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (IMPLIES (NOT x1) x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFFI (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 x1) (IMPLIES (IMPLIES x1 x0) (EQUIV x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x0 (IMPLIES x1 (EQUIV x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x0) (IMPLIES (NOT x1) (EQUIV x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFFE1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFFE2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _COMIFF (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (EQUIV x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (NOT x0) (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (NOT x1) (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH7 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES x0 (IMPLIES (NOT x1) (NOT (EQUIV x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH8 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (NOT x0) (IMPLIES x1 (NOT (EQUIV x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFIFF (in landau0)
 (definition (FORALL (LAM (x0 O) (EQUIV x0 x0))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SYMIFF (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (EQUIV x1 x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRIFF (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (EQUIV x1 x2) (EQUIV x0 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH9 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (NOT x0) (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH10 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (NOT x1) (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH11 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 x1) (EQUIV (NOT x0) (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH12 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES (NOT x0) (NOT x1)) (IMPLIES (IMPLIES (NOT x1) (NOT x0)) (EQUIV x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH13 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (EQUIV x0 (NOT x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH14 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (_OREC x0 x1) (EQUIV x1 (NOT x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH15 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x0 (NOT x1)) (_OREC x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IFF_TH16 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (EQUIV x1 (NOT x0)) (_OREC x0 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THIMP1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (IMPLIES x0 x2) (IMPLIES x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THIMP2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (IMPLIES x2 x0) (IMPLIES x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THEC1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (IMPLIES x0 (NOT x2)) (IMPLIES x1 (NOT x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THEC2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (IMPLIES x2 (NOT x0)) (IMPLIES x2 (NOT x1)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THAND1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (AND x0 x2) (AND x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THAND2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (AND x2 x0) (AND x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THOR1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (OR x0 x2) (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THOR2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (OR x2 x0) (OR x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THOREC1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (_OREC x0 x2) (_OREC x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THOREC2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (EQUIV x0 x1) (IMPLIES (_OREC x2 x0) (_OREC x2 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ALLE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2)))) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (x1 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ALL_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (NOT (x1 x2)) (NOT (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOMEI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x1 x2) (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (NOT (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2))))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (NOT (NOT (x1 x3)))))) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (x1 x4)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (NOT (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2))))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (NOT (NOT (x1 x3)))))) FALSE)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOME_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (NOT (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2))))) (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (NOT (x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (NOT (x1 x2))))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (x1 x3)))) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (NOT (NOT (x1 x4)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (NOT (x1 x2))))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (x1 x3)))) FALSE)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOME_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (NOT (x1 x2))))) (NOT (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOME_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (NOT (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2))))) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (NOT (x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOME_TH4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (NOT (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2))))) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (NOT (x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOME_TH5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (NOT (x1 x2))))) (NOT (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (x1 x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2)))) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x4) x3)))) (IMPLIES (NOT x3) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (NOT (x1 x5)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2)))) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x4) x3)))) (IMPLIES (NOT x3) FALSE))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOMEAPP (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2)))) (FORALL (LAM (x3 O) (IMPLIES (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x4) x3)))) x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOME_TH6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 (O Z)) (IMPLIES (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (x1 x3)))) (IMPLIES (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x4) (x2 x4))))) (EXISTS (LAM (x5 Z) (AND (x0 x5 x5) (x2 x5)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (NOT x0) (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3E3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (NOT x0) (IMPLIES (NOT x1) x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (NOT x1) (OR x2 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3E1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (NOT x1) (IMPLIES (NOT x2) x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (NOT x2) (OR x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3E2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (NOT x2) (IMPLIES (NOT x0) x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (OR x1 (OR x2 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (OR x2 (OR x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3I1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES x0 (OR x0 (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3I2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES x1 (OR x0 (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3I3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES x2 (OR x0 (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 x1) (OR x0 (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH7 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x1 x2) (OR x0 (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3_TH8 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x2 x0) (OR x0 (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OR3APP (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (FORALL (LAM (x3 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (IMPLIES x0 x3) (IMPLIES (IMPLIES x1 x3) (IMPLIES (IMPLIES x2 x3) x3)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3E1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) x0))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3E2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3E3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) x2))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3I (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES x0 (IMPLIES x1 (IMPLIES x2 (AND x0 (AND x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) (AND x1 (AND x2 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) (AND x2 (AND x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) (AND x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) (AND x1 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3_TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) (AND x2 x0)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AND3_TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND x0 (AND x1 x2)) (AND x2 (AND x1 x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x0 (NOT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x1 (NOT x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x2 (NOT x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH4 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (AND (IMPLIES x1 (NOT x2)) (AND (IMPLIES x2 (NOT x0)) (IMPLIES x0 (NOT x1))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH5 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (AND (IMPLIES x2 (NOT x0)) (AND (IMPLIES x0 (NOT x1)) (IMPLIES x1 (NOT x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TH5A (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (AND (IMPLIES x2 (NOT x1)) (AND (IMPLIES x1 (NOT x0)) (IMPLIES x0 (NOT x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3E12 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x0 (NOT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3E13 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x0 (NOT x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3E23 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x1 (NOT x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3E21 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x1 (NOT x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3E31 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x2 (NOT x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3E32 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES x2 (NOT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH6 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (IMPLIES x0 (NOT x1)) (IMPLIES (IMPLIES x1 (NOT x2)) (IMPLIES (IMPLIES x2 (NOT x0)) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH7 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES (OR x0 x1) (NOT x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH8 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES (OR x1 x2) (NOT x0))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH9 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (IMPLIES (OR x2 x0) (NOT x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3I1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (NOT x0) (IMPLIES (NOT x1) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3I2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (NOT x1) (IMPLIES (NOT x2) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3I3 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (NOT x2) (IMPLIES (NOT x0) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_T1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (FORALL (LAM (x3 O) (FORALL (LAM (x4 O) (FORALL (LAM (x5 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (AND (IMPLIES x3 (NOT x4)) (AND (IMPLIES x4 (NOT x5)) (IMPLIES x5 (NOT x3)))) (IMPLIES (IMPLIES x0 x3) (IMPLIES (IMPLIES x1 x4) (IMPLIES (IMPLIES x2 x5) (IMPLIES x3 (NOT x1))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_T2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (FORALL (LAM (x3 O) (FORALL (LAM (x4 O) (FORALL (LAM (x5 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (AND (IMPLIES x3 (NOT x4)) (AND (IMPLIES x4 (NOT x5)) (IMPLIES x5 (NOT x3)))) (IMPLIES (IMPLIES x0 x3) (IMPLIES (IMPLIES x1 x4) (IMPLIES (IMPLIES x2 x5) (IMPLIES x3 (NOT x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH10 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (FORALL (LAM (x3 O) (FORALL (LAM (x4 O) (FORALL (LAM (x5 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (AND (IMPLIES x3 (NOT x4)) (AND (IMPLIES x4 (NOT x5)) (IMPLIES x5 (NOT x3)))) (IMPLIES (IMPLIES x0 x3) (IMPLIES (IMPLIES x1 x4) (IMPLIES (IMPLIES x2 x5) (IMPLIES x3 x0)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH11 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (FORALL (LAM (x3 O) (FORALL (LAM (x4 O) (FORALL (LAM (x5 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (AND (IMPLIES x3 (NOT x4)) (AND (IMPLIES x4 (NOT x5)) (IMPLIES x5 (NOT x3)))) (IMPLIES (IMPLIES x0 x3) (IMPLIES (IMPLIES x1 x4) (IMPLIES (IMPLIES x2 x5) (IMPLIES x4 x1)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EC3_TH12 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (FORALL (LAM (x3 O) (FORALL (LAM (x4 O) (FORALL (LAM (x5 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (AND (IMPLIES x3 (NOT x4)) (AND (IMPLIES x4 (NOT x5)) (IMPLIES x5 (NOT x3)))) (IMPLIES (IMPLIES x0 x3) (IMPLIES (IMPLIES x1 x4) (IMPLIES (IMPLIES x2 x5) (IMPLIES x5 x2)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC3E1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (OR x0 (OR x1 x2)) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0))))) (OR x0 (OR x1 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC3E2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (OR x0 (OR x1 x2)) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0))))) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC3I (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (OR x0 (OR x1 x2)) (IMPLIES (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0)))) (AND (OR x0 (OR x1 x2)) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC3_TH1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (OR x0 (OR x1 x2)) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0))))) (AND (OR x1 (OR x2 x0)) (AND (IMPLIES x1 (NOT x2)) (AND (IMPLIES x2 (NOT x0)) (IMPLIES x0 (NOT x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OREC3_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (FORALL (LAM (x2 O) (IMPLIES (AND (OR x0 (OR x1 x2)) (AND (IMPLIES x0 (NOT x1)) (AND (IMPLIES x1 (NOT x2)) (IMPLIES x2 (NOT x0))))) (AND (OR x2 (OR x0 x1)) (AND (IMPLIES x2 (NOT x0)) (AND (IMPLIES x0 (NOT x1)) (IMPLIES x1 (NOT x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFIS (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (x0 x1 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISP (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x1 x2) (IMPLIES (x0 x2 x3) (x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SYMIS (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x0 x1 x2) (x0 x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRIS (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x1 x2) (IMPLIES (x0 x2 x3) (x0 x1 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRIS1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (IMPLIES (x0 x3 x2) (x0 x1 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRIS2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x1 x3) (IMPLIES (x0 x2 x3) (x0 x1 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISP1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x1 x2) (IMPLIES (x0 x3 x2) (x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SYMNOTIS (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (NOT (x0 x1 x2)) (NOT (x0 x2 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOTIS_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x1 x2)) (IMPLIES (x0 x1 x3) (NOT (x0 x3 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOTIS_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x1 x2)) (IMPLIES (x0 x3 x1) (NOT (x0 x3 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOTIS_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x1 x2)) (IMPLIES (x0 x2 x3) (NOT (x0 x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOTIS_TH4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x1 x2)) (IMPLIES (x0 x3 x2) (NOT (x0 x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NOTIS_TH5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (NOT (x0 x1 x2)) (IMPLIES (x0 x1 x3) (IMPLIES (x0 x2 x4) (NOT (x0 x3 x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TR3IS (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 x1 x2) (IMPLIES (x0 x2 x3) (IMPLIES (x0 x3 x4) (x0 x1 x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TR4IS (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (IMPLIES (x0 x1 x2) (IMPLIES (x0 x2 x3) (IMPLIES (x0 x3 x4) (IMPLIES (x0 x4 x5) (x0 x1 x5)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AMONE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x1 x2) (IMPLIES (x1 x3) (x0 x2 x3))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _AMONEE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_AMONE x0 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x1 x2) (IMPLIES (x1 x3) (x0 x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _E_ONE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (AND (_AMONE x0 x1) (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ONEI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_AMONE x0 x1) (IMPLIES (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2)))) (_E_ONE x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ONEE1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_E_ONE x0 x1) (_AMONE x0 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ONEE2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_E_ONE x0 x1) (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ONEAX (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_E_ONE x0 x1) (x1 (THAT (LAM (x2 Z) (AND (x0 x2 x2) (x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ONE_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_E_ONE x0 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x1 x2) (x0 (THAT (LAM (x3 Z) (AND (x0 x3 x3) (x1 x3)))) x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISF (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (IMPLIES (x0 x5 x6) (x1 (x2 x5) (x2 x6))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJECTIVE (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Z Z)) (LAM (x2 (Z Y)) (FORALL (LAM (x3 Y) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Y) (IMPLIES (x0 x4 x4) (IMPLIES (x1 (x2 x3) (x2 x4)) (x0 x3 x4))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISFE (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (IMPLIES (x1 (x2 x5) (x2 x6)) (x0 x5 x6))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IMAGE (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Z Z)) (LAM (x2 (Z Y)) (LAM (x3 Z) (EXISTS (LAM (x4 Y) (AND (x0 x4 x4) (x1 x3 (x2 x4))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IMAGEI (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (_IMAGE x0 x1 x2 (x2 x5))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_T1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (FORALL (LAM (x6 Y) (IMPLIES (x1 x6 x6) (IMPLIES (x1 x5 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (IMPLIES (x1 x5 (x2 x7)) (IMPLIES (x1 x6 (x2 x8)) (x1 (x2 x7) (x2 x8)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_TH1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (FORALL (LAM (x6 Y) (IMPLIES (x1 x6 x6) (IMPLIES (x1 x5 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (IMPLIES (x1 x5 (x2 x7)) (IMPLIES (x1 x6 (x2 x8)) (x0 x7 x8))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_TH2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (_AMONE x0 (LAM (x6 Z) (x1 x5 (x2 x6)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_TH3 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (IMPLIES (_IMAGE x0 x1 x2 x5) (_E_ONE x0 (LAM (x6 Z) (x1 x5 (x2 x6))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOFT (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Y Y)) (LAM (x2 (Y Z)) (LAM (x3 Y) (THAT (LAM (x4 Z) (AND (x0 x4 x4) (x1 x3 (x2 x4))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SOFT_DEC (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x2 x4) (x3 x5))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x6 Y) (FORALL (LAM (x7 Y) (IMPLIES (x1 x6 x7) (IMPLIES (_IMAGE x0 x1 x2 x6) (x0 (_SOFT x0 x1 x2 x6) (_SOFT x0 x1 x3 x7))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVERSE (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition _SOFT)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVERSE_DEC (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x2 x4) (x3 x5))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x6 Y) (FORALL (LAM (x7 Y) (IMPLIES (x1 x6 x7) (IMPLIES (_IMAGE x0 x1 x2 x6) (x0 (_INVERSE x0 x1 x2 x6) (_INVERSE x0 x1 x3 x7))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISTS1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (IMPLIES (_IMAGE x0 x1 x2 x5) (x1 x5 (x2 (_SOFT x0 x1 x2 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISTS2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (IMPLIES (_IMAGE x0 x1 x2 x5) (x1 (x2 (_SOFT x0 x1 x2 x5)) x5)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINV (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (IMPLIES (_IMAGE x0 x1 x2 x5) (FORALL (LAM (x6 Y) (IMPLIES (x1 x6 x6) (IMPLIES (_IMAGE x0 x1 x2 x6) (IMPLIES (x1 x5 x6) (x0 (_SOFT x0 x1 x2 x5) (_SOFT x0 x1 x2 x6)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINVE (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (IMPLIES (_IMAGE x0 x1 x2 x5) (FORALL (LAM (x6 Y) (IMPLIES (x1 x6 x6) (IMPLIES (_IMAGE x0 x1 x2 x6) (IMPLIES (x0 (_SOFT x0 x1 x2 x5) (_SOFT x0 x1 x2 x6)) (x1 x5 x6))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISST1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (x0 x5 (_SOFT x0 x1 x2 (x2 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISST2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_INJECTIVE x0 x1 x2) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (x0 (_SOFT x0 x1 x2 (x2 x5)) x5))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJECTIVE (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Z Z)) (LAM (x2 (Z Y)) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (_IMAGE x0 x1 x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _BIJECTIVE (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Z Z)) (LAM (x2 (Z Y)) (AND (_INJECTIVE x0 x1 x2) (_SURJECTIVE x0 x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_T2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_BIJECTIVE x0 x1 x2) (_INJECTIVE x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_T3 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_BIJECTIVE x0 x1 x2) (_SURJECTIVE x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SO (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition _SOFT)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SO_DEC (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x2 x4) (x3 x5))))))) (IMPLIES (_BIJECTIVE x0 x1 x2) (FORALL (LAM (x6 Y) (FORALL (LAM (x7 Y) (IMPLIES (x1 x6 x7) (x0 (_SO x0 x1 x2 x6) (_SO x0 x1 x3 x7)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVF (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition _SO)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INVF_DEC (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x2 x4) (x3 x5))))))) (IMPLIES (_BIJECTIVE x0 x1 x2) (FORALL (LAM (x6 Y) (FORALL (LAM (x7 Y) (IMPLIES (x1 x6 x7) (x0 (_INVF x0 x1 x2 x6) (_INVF x0 x1 x3 x7)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THINVF1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_BIJECTIVE x0 x1 x2) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (x0 x5 (_INVF x0 x1 x2 (x2 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _THINVF2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (IMPLIES (_BIJECTIVE x0 x1 x2) (FORALL (LAM (x5 Y) (IMPLIES (x1 x5 x5) (x1 x5 (x2 (_INVF x0 x1 x2 x5))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_T4 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_INJECTIVE x0 x1 x3) (IMPLIES (_INJECTIVE x1 x2 x6) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (x2 (x6 (x3 x9)) (x6 (x3 x10))) (x1 (x3 x9) (x3 x10))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_T5 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_INJECTIVE x0 x1 x3) (IMPLIES (_INJECTIVE x1 x2 x6) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (x2 (x6 (x3 x9)) (x6 (x3 x10))) (x0 x9 x10)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INJ_TH4 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_INJECTIVE x0 x1 x3) (IMPLIES (_INJECTIVE x1 x2 x6) (_INJECTIVE x0 x2 (LAM (x9 Z) (x6 (x3 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJ_T1 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_SURJECTIVE x0 x1 x3) (IMPLIES (_SURJECTIVE x1 x2 x6) (FORALL (LAM (x9 X) (IMPLIES (x2 x9 x9) (_IMAGE x1 x2 x6 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJ_T2 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_SURJECTIVE x0 x1 x3) (IMPLIES (_SURJECTIVE x1 x2 x6) (FORALL (LAM (x9 X) (IMPLIES (x2 x9 x9) (FORALL (LAM (x10 Y) (IMPLIES (x1 x10 x10) (IMPLIES (x2 x9 (x6 x10)) (_IMAGE x0 x1 x3 x10)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJ_T3 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_SURJECTIVE x0 x1 x3) (IMPLIES (_SURJECTIVE x1 x2 x6) (FORALL (LAM (x9 X) (IMPLIES (x2 x9 x9) (FORALL (LAM (x10 Y) (IMPLIES (x1 x10 x10) (IMPLIES (x2 x9 (x6 x10)) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (x1 x10 (x3 x11)) (x2 x9 (x6 (x3 x11)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJ_T4 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_SURJECTIVE x0 x1 x3) (IMPLIES (_SURJECTIVE x1 x2 x6) (FORALL (LAM (x9 X) (IMPLIES (x2 x9 x9) (FORALL (LAM (x10 Y) (IMPLIES (x1 x10 x10) (IMPLIES (x2 x9 (x6 x10)) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (x1 x10 (x3 x11)) (_IMAGE x0 x2 (LAM (x12 Z) (x6 (x3 x12))) x9)))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJ_T5 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_SURJECTIVE x0 x1 x3) (IMPLIES (_SURJECTIVE x1 x2 x6) (FORALL (LAM (x9 X) (IMPLIES (x2 x9 x9) (FORALL (LAM (x10 Y) (IMPLIES (x1 x10 x10) (IMPLIES (x2 x9 (x6 x10)) (_IMAGE x0 x2 (LAM (x11 Z) (x6 (x3 x11))) x9)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJ_T6 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_SURJECTIVE x0 x1 x3) (IMPLIES (_SURJECTIVE x1 x2 x6) (FORALL (LAM (x9 X) (IMPLIES (x2 x9 x9) (_IMAGE x0 x2 (LAM (x10 Z) (x6 (x3 x10))) x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SURJ_TH1 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_SURJECTIVE x0 x1 x3) (IMPLIES (_SURJECTIVE x1 x2 x6) (_SURJECTIVE x0 x2 (LAM (x9 Z) (x6 (x3 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _BIJ_T1 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_BIJECTIVE x0 x1 x3) (IMPLIES (_BIJECTIVE x1 x2 x6) (_INJECTIVE x0 x2 (LAM (x9 Z) (x6 (x3 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _BIJ_T2 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_BIJECTIVE x0 x1 x3) (IMPLIES (_BIJECTIVE x1 x2 x6) (_SURJECTIVE x0 x2 (LAM (x9 Z) (x6 (x3 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _BIJ_TH1 (in landau0)
 (TYPE-VARIABLES X Y Z)
 (definition (LAM (DUMMY (X Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O X X)) (IMPLIES (PER x2) (FORALL (LAM (x3 (Y Z)) (IMPLIES (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x0 x4 x5) (x1 (x3 x4) (x3 x5))))))) (FORALL (LAM (x6 (X Y)) (IMPLIES (FORALL (LAM (x7 Y) (FORALL (LAM (x8 Y) (IMPLIES (x1 x7 x8) (x2 (x6 x7) (x6 x8))))))) (IMPLIES (_BIJECTIVE x0 x1 x3) (IMPLIES (_BIJECTIVE x1 x2 x6) (_BIJECTIVE x0 x2 (LAM (x9 Z) (x6 (x3 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FISE (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (Y Z)) (IMPLIES (FORALL (LAM (x6 Z) (FORALL (LAM (x7 Z) (IMPLIES (x0 x6 x7) (x1 (x5 x6) (x5 x7))))))) (IMPLIES (FORALL (LAM (x8 Z) (FORALL (LAM (x9 Z) (IMPLIES (x0 x8 x9) (x1 (x2 x8) (x5 x9))))))) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (x1 (x2 x10) (x5 x10))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FISI (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (Y Z)) (IMPLIES (FORALL (LAM (x6 Z) (FORALL (LAM (x7 Z) (IMPLIES (x0 x6 x7) (x1 (x5 x6) (x5 x7))))))) (IMPLIES (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (x1 (x2 x8) (x5 x8))))) (FORALL (LAM (x9 Z) (FORALL (LAM (x10 Z) (IMPLIES (x0 x9 x10) (x1 (x2 x9) (x5 x10))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIS_TH1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 (Y Z)) (IMPLIES (FORALL (LAM (x6 Z) (FORALL (LAM (x7 Z) (IMPLIES (x0 x6 x7) (x1 (x5 x6) (x5 x7))))))) (IMPLIES (FORALL (LAM (x8 Z) (FORALL (LAM (x9 Z) (IMPLIES (x0 x8 x9) (x1 (x2 x8) (x5 x9))))))) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (x0 x10 x11) (x1 (x2 x10) (x5 x11))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OT (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (x0 (O A A)) (LAM (x1 (O A)) (LAM (x2 A) (LAM (x3 A) (AND (AND (x0 x2 x3) (x1 x2)) (x1 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OT_DEC (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (DUMMY A) (FORALL (LAM (x0 (O A A)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O A)) (IMPLIES TRUE (PER (_OT x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INP (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (_OT x0 x1 x2 x2) (x1 x2)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OTAX1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (_INJECTIVE (_OT x0 x1) x0 (LAM (x2 Z) x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OTAX2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x1 x2) (_IMAGE (_OT x0 x1) x0 (LAM (x3 Z) x3) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (_OT x0 x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (_OT x0 x1 x3 x3) (IMPLIES (_OT x0 x1 x2 x3) (x0 x2 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (_OT x0 x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (_OT x0 x1 x3 x3) (IMPLIES (x0 x2 x3) (_OT x0 x1 x2 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OUT (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (_SOFT (_OT x0 x1) x0 (LAM (x2 Z) x2)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _OUT_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES TRUE (FORALL (LAM (x2 Z) (FORALL (LAM (x3 Z) (IMPLIES (x0 x2 x3) (IMPLIES (x1 x2) (_OT x0 x1 (_OUT x0 x1 x2) (_OUT x0 x1 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISOUTI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x1 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x1 x3) (IMPLIES (x0 x2 x3) (_OT x0 x1 (_OUT x0 x1 x2) (_OUT x0 x1 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISOUTE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x1 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x1 x3) (IMPLIES (_OT x0 x1 (_OUT x0 x1 x2) (_OUT x0 x1 x3)) (x0 x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISOUTIN (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (_OT x0 x1 x2 x2) (_OT x0 x1 x2 (_OUT x0 x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISINOUT (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x1 x2) (x0 x2 (_OUT x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _PROP1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 O) (LAM (x1 (O Z Z)) (LAM (x2 Z) (LAM (x3 Z) (LAM (x4 Z) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (IMPLIES x0 (x1 x4 x2))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (x1 x4 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (x1 x5 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (x1 x4 x5)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (_AMONE x1 (_PROP1 x0 x1 x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T7 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (IMPLIES (NOT x0) (x1 x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T8 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (_PROP1 x0 x1 x2 x3 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T9 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (EXISTS (LAM (x4 Z) (AND (x1 x4 x4) (_PROP1 x0 x1 x2 x3 x4))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T10 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (_E_ONE x1 (_PROP1 x0 x1 x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T11 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (IMPLIES (NOT x0) (x1 x4 x3))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T12 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (x1 x4 x3)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T13 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (x1 x5 x3)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T14 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (_PROP1 x0 x1 x2 x3 x4) (IMPLIES (_PROP1 x0 x1 x2 x3 x5) (x1 x4 x5)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T15 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (_AMONE x1 (_PROP1 x0 x1 x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T16 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T17 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (IMPLIES x0 (x1 x3 x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T18 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (_PROP1 x0 x1 x2 x3 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T19 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (EXISTS (LAM (x4 Z) (AND (x1 x4 x4) (_PROP1 x0 x1 x2 x3 x4))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T20 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (_E_ONE x1 (_PROP1 x0 x1 x2 x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T21 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (_E_ONE x1 (_PROP1 x0 x1 x2 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 O) (LAM (x1 (O Z Z)) (LAM (x2 Z) (LAM (x3 Z) (THAT (LAM (x4 Z) (AND (x1 x4 x4) (_PROP1 x0 x1 x2 x3 x4)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (IMPLIES TRUE (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (FORALL (LAM (x3 Z) (IMPLIES (x1 x2 x3) (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (x1 x4 x5) (x1 (_ITE x0 x1 x2 x4) (_ITE x0 x1 x3 x5))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T22 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (_PROP1 x0 x1 x2 x3 (_ITE x0 x1 x2 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T23 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (x1 (_ITE x0 x1 x2 x3) x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T24 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (x1 (_ITE x0 x1 x2 x3) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITET (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES x0 (x1 (_ITE x0 x1 x2 x3) x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITEF (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (x1 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x1 x3 x3) (IMPLIES (NOT x0) (x1 (_ITE x0 x1 x2 x3) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WA (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 Z) (LAM (x2 Z) (LAM (x3 Z) (_ITE (x0 x3 x1) x0 x2 x3))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WA_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (FORALL (LAM (x2 Z) (IMPLIES (x0 x1 x2) (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (FORALL (LAM (x5 Z) (FORALL (LAM (x6 Z) (IMPLIES (x0 x5 x6) (x0 (_WA x0 x1 x3 x5) (_WA x0 x2 x4 x6))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (x0 (_WA x0 x1 x2 x3) x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x3 x1)) (x0 (_WA x0 x1 x2 x3) x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WB (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 Z) (LAM (x2 Z) (LAM (x3 Z) (_ITE (x0 x3 x2) x0 x1 (_WA x0 x1 x2 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WB_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (FORALL (LAM (x2 Z) (IMPLIES (x0 x1 x2) (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (FORALL (LAM (x5 Z) (FORALL (LAM (x6 Z) (IMPLIES (x0 x5 x6) (x0 (_WB x0 x1 x3 x5) (_WB x0 x2 x4 x6))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x2) (x0 (_WB x0 x1 x2 x3) x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x3 x2)) (x0 (_WB x0 x1 x2 x3) (_WA x0 x1 x2 x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (IMPLIES (x0 x1 x2) (x0 (_WB x0 x1 x2 x3) x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (IMPLIES (NOT (x0 x1 x2)) (x0 (_WB x0 x1 x2 x3) x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T7 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (x0 (_WB x0 x1 x2 x3) x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T8 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x3 x1)) (IMPLIES (NOT (x0 x3 x2)) (x0 (_WB x0 x1 x2 x3) x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL (in landau0)
 (TYPE-VARIABLES Z)
 (definition _WB)
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (FORALL (LAM (x2 Z) (IMPLIES (x0 x1 x2) (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (FORALL (LAM (x5 Z) (FORALL (LAM (x6 Z) (IMPLIES (x0 x5 x6) (x0 (_WISSEL x0 x1 x3 x5) (_WISSEL x0 x2 x4 x6))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISWISSEL1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (x0 (_WISSEL x0 x1 x2 x3) x2))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISWISSEL2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x2) (x0 (_WISSEL x0 x1 x2 x3) x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISWISSEL3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x3 x1)) (IMPLIES (NOT (x0 x3 x2)) (x0 (_WISSEL x0 x1 x2 x3) x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T9 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (NOT (x0 x4 x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T10 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (IMPLIES (x0 x1 x2) (NOT (x0 x4 x2)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T11 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (IMPLIES (x0 x1 x2) (x0 (_WB x0 x1 x2 x3) x4))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T12 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (IMPLIES (x0 x1 x2) FALSE)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T13 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (NOT (x0 x1 x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T14 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (IMPLIES (x0 x4 x2) (x0 (_WB x0 x1 x2 x3) x1))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T15 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (IMPLIES (x0 x4 x2) FALSE)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T16 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (NOT (x0 x4 x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T17 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) (x0 (_WB x0 x1 x2 x3) x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T18 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x1) FALSE))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T19 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (NOT (x0 x3 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T20 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (NOT (x0 x4 x1)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T21 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x2) (NOT (x0 x4 x2))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T22 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x2) (x0 (_WB x0 x1 x2 x3) x4)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T23 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (IMPLIES (x0 x3 x2) FALSE))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_T24 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (NOT (x0 x3 x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T25 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (NOT (x0 x4 x2)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T26 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) (x0 (_WB x0 x1 x2 x3) x4))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T27 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (IMPLIES (NOT (x0 x3 x4)) FALSE)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T28 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x0 (_WB x0 x1 x2 x3) (_WB x0 x1 x2 x4)) (x0 x3 x4)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (_INJECTIVE x0 x0 (_WISSEL x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T29 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (x0 x3 (_WB x0 x1 x2 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T30 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x1) (_IMAGE x0 x0 (_WISSEL x0 x1 x2) x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T31 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x2) (x0 x3 (_WB x0 x1 x2 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T32 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (x0 x3 x2) (_IMAGE x0 x0 (_WISSEL x0 x1 x2) x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T33 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x3 x1)) (IMPLIES (NOT (x0 x3 x2)) (x0 x3 (_WB x0 x1 x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T34 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x3 x1)) (IMPLIES (NOT (x0 x3 x2)) (_IMAGE x0 x0 (_WISSEL x0 x1 x2) x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T35 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (x0 x3 x1)) (_IMAGE x0 x0 (_WISSEL x0 x1 x2) x3))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _T36 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (_IMAGE x0 x0 (_WISSEL x0 x1 x2) x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (_SURJECTIVE x0 x0 (_WISSEL x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 Z) (IMPLIES (x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (_BIJECTIVE x0 x0 (_WISSEL x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CHANGEF (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Z Z)) (LAM (x2 (Z Y)) (LAM (x3 Y) (LAM (x4 Y) (LAM (x5 Y) (x2 (_WISSEL x0 x3 x4 x5)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CHANGEF_DEC (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (DUMMY (Z Y)) (FORALL (LAM (x0 (O Y Y)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Z Y)) (FORALL (LAM (x3 (Z Y)) (IMPLIES (FORALL (LAM (x4 Y) (FORALL (LAM (x5 Y) (IMPLIES (x0 x4 x5) (x1 (x2 x4) (x3 x5))))))) (FORALL (LAM (x6 Y) (FORALL (LAM (x7 Y) (IMPLIES (x0 x6 x7) (FORALL (LAM (x8 Y) (FORALL (LAM (x9 Y) (IMPLIES (x0 x8 x9) (FORALL (LAM (x10 Y) (FORALL (LAM (x11 Y) (IMPLIES (x0 x10 x11) (x1 (_CHANGEF x0 x1 x2 x6 x8 x10) (_CHANGEF x0 x1 x3 x7 x9 x11))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CHANGEF1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x0 x7 x5) (x1 (_CHANGEF x0 x1 x2 x5 x6 x7) (x2 x6)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CHANGEF2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x0 x7 x6) (x1 (_CHANGEF x0 x1 x2 x5 x6 x7) (x2 x5)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _CHANGEF3 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (NOT (x0 x7 x5)) (IMPLIES (NOT (x0 x7 x6)) (x1 (_CHANGEF x0 x1 x2 x5 x6 x7) (x2 x7))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_TH4 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (IMPLIES (_INJECTIVE x0 x1 x2) (_INJECTIVE x0 x1 (_CHANGEF x0 x1 x2 x5 x6))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_TH5 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (IMPLIES (_SURJECTIVE x0 x1 x2) (_SURJECTIVE x0 x1 (_CHANGEF x0 x1 x2 x5 x6))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _WISSEL_TH6 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (Y Z)) (IMPLIES (FORALL (LAM (x3 Z) (FORALL (LAM (x4 Z) (IMPLIES (x0 x3 x4) (x1 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (IMPLIES (_BIJECTIVE x0 x1 x2) (_BIJECTIVE x0 x1 (_CHANGEF x0 x1 x2 x5 x6))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_MP (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 TRUE) (IMPLIES x0 (IMPLIES (IMPLIES x0 x1) x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _IMP_TH2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 TRUE) (IMPLIES (NOT x0) (IMPLIES x0 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ECI1 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 TRUE) (IMPLIES (NOT x0) (IMPLIES x0 (NOT x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ANDE2 (in landau0)
 (definition (FORALL (LAM (x0 O) (FORALL (LAM (x1 O) (IMPLIES (IMPLIES x0 TRUE) (IMPLIES (AND x0 (IMPLIES x0 x1)) x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE_T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (IMPLIES x0 (x1 x4 x2))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE_T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (x1 x4 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE_T3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (x1 x5 x2)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE_T4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (x1 x4 x5)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE_T5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (_AMONE x1 (LAM (x4 Z) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE_T6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T7 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (IMPLIES (NOT x0) (x1 x2 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T8 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (AND (IMPLIES x0 (x1 x2 x2)) (IMPLIES (NOT x0) (x1 x2 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T9 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (EXISTS (LAM (x4 Z) (AND (x1 x4 x4) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T10 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (_E_ONE x1 (LAM (x4 Z) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T11 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (IMPLIES (NOT x0) (x1 x4 x3))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T12 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (x1 x4 x3)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T13 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (x1 x5 x3)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T14 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (FORALL (LAM (x4 Z) (IMPLIES (x1 x4 x4) (FORALL (LAM (x5 Z) (IMPLIES (x1 x5 x5) (IMPLIES (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))) (IMPLIES (AND (IMPLIES x0 (x1 x5 x2)) (IMPLIES (NOT x0) (x1 x5 x3))) (x1 x4 x5)))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T15 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (_AMONE x1 (LAM (x4 Z) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T16 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T17 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (IMPLIES x0 (x1 x3 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T18 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (AND (IMPLIES x0 (x1 x3 x2)) (IMPLIES (NOT x0) (x1 x3 x3)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T19 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (EXISTS (LAM (x4 Z) (AND (x1 x4 x4) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T20 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (_E_ONE x1 (LAM (x4 Z) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T21 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (_E_ONE x1 (LAM (x4 Z) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 O) (LAM (x1 (O Z Z)) (LAM (x2 Z) (LAM (x3 Z) (THAT (LAM (x4 Z) (AND (x1 x4 x4) (AND (IMPLIES x0 (x1 x4 x2)) (IMPLIES (NOT x0) (x1 x4 x3)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITE_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (IMPLIES TRUE (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES x0 (x1 x2 x3)) (FORALL (LAM (x4 Z) (FORALL (LAM (x5 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x4 x5)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x4 x4))) (x1 (_R_ITE x0 x1 x2 x4) (_R_ITE x0 x1 x3 x5))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T22 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (AND (IMPLIES x0 (x1 (_R_ITE x0 x1 x2 x3) x2)) (IMPLIES (NOT x0) (x1 (_R_ITE x0 x1 x2 x3) x3))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T23 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (x1 (_R_ITE x0 x1 x2 x3) x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ITE_T24 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (x1 (_R_ITE x0 x1 x2 x3) x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITET (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES x0 (x1 (_R_ITE x0 x1 x2 x3) x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _R_ITEF (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 O) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (PER x1) (FORALL (LAM (x2 Z) (IMPLIES (IMPLIES x0 (x1 x2 x2)) (FORALL (LAM (x3 Z) (IMPLIES (IMPLIES (NOT x0) (x1 x3 x3)) (IMPLIES (IMPLIES x0 (IMPLIES x0 (x1 x2 x2))) (IMPLIES (IMPLIES (NOT x0) (IMPLIES (NOT x0) (x1 x3 x3))) (IMPLIES (NOT x0) (x1 (_R_ITE x0 x1 x2 x3) x3)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SET (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (x0 (O A A)) (LAM (x1 (O A)) (LAM (x2 (O A)) (FORALL (LAM (x3 A) (FORALL (LAM (x4 A) (IMPLIES (x0 x3 x4) (EQUIV (x1 x3) (x2 x4)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SET_DEC (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (DUMMY A) (FORALL (LAM (x0 (O A A)) (IMPLIES (PER x0) (PER (_SET x0)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ESTI (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (x0 (O A A)) (LAM (x1 A) (LAM (x2 (O A)) (x2 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SETOF (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (x0 (O A A)) (LAM (x1 (O A)) x1)))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SETOF_DEC (in landau0)
 (TYPE-VARIABLES A)
 (definition (LAM (DUMMY A) (FORALL (LAM (x0 (O A A)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O A)) (IMPLIES TRUE (_SET x0 (_SETOF x0 x1) (_SETOF x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ESTII (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (x1 x2) (_ESTI x0 x2 (_SETOF x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ESTIE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (_ESTI x0 x2 (_SETOF x0 x1)) (x1 x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EMPTY (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (NOT (_ESTI x0 x2 x1))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NONEMPTY (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (EXISTS (LAM (x2 Z) (AND (x0 x2 x2) (_ESTI x0 x2 x1)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EMPTYI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (NOT (_ESTI x0 x2 x1))))) (_EMPTY x0 x1))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EMPTYE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (IMPLIES (_EMPTY x0 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (NOT (_ESTI x0 x2 x1))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NONEMPTYI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (IMPLIES (_ESTI x0 x2 x1) (_NONEMPTY x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NONEMPTYAPP (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (IMPLIES (_NONEMPTY x0 x1) (FORALL (LAM (x2 O) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) x2)))) x2))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INCL (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (LAM (x2 (O Z)) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (_ESTI x0 x3 x2)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INCLI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (_ESTI x0 x3 x2))))) (_INCL x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INCLE (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_INCL x0 x1 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (_ESTI x0 x3 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFINCL (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (_INCL x0 x1 x1)))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISJ (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (LAM (x2 (O Z)) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (NOT (_ESTI x0 x3 x2))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISJI1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (NOT (_ESTI x0 x3 x2)))))) (_DISJ x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISJI2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x2) (NOT (_ESTI x0 x3 x1)))))) (_DISJ x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISJE1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_DISJ x0 x1 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (NOT (_ESTI x0 x3 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISJE2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_DISJ x0 x1 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x2) (NOT (_ESTI x0 x3 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SYMDISJ (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_DISJ x0 x1 x2) (_DISJ x0 x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISJ_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (IMPLIES (_ESTI x0 x3 x2) (NOT (_DISJ x0 x1 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _DISJ_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (IMPLIES (_ESTI x0 x3 x2) (NOT (_DISJ x0 x2 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSETE1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_SET x0 x1 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (_ESTI x0 x3 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSETE2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_SET x0 x1 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x2) (_ESTI x0 x3 x1)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_SET x0 x1 x2) (_INCL x0 x1 x2)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_SET x0 x1 x2) (_INCL x0 x2 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSETI (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (_INCL x0 x1 x2) (IMPLIES (_INCL x0 x2 x1) (_SET x0 x1 x2))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (IMPLIES (NOT (_ESTI x0 x3 x2)) (NOT (_SET x0 x1 x2))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_TH4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (_ESTI x0 x3 x1) (IMPLIES (NOT (_ESTI x0 x3 x2)) (NOT (_SET x0 x2 x1))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _NISSETPROP (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z)) (LAM (x2 (O Z)) (LAM (x3 Z) (AND (_ESTI x0 x3 x1) (NOT (_ESTI x0 x3 x2))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (IMPLIES (NOT (_NISSETPROP x0 x1 x2 x3)) (IMPLIES (_ESTI x0 x3 x1) (_ESTI x0 x3 x2)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (NOT (_SET x0 x1 x2)) (IMPLIES (NOT (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (_NISSETPROP x0 x1 x2 x3))))) (IMPLIES (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (NOT (_NISSETPROP x0 x2 x1 x4))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (NOT (_NISSETPROP x0 x1 x2 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_T3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (NOT (_SET x0 x1 x2)) (IMPLIES (NOT (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (_NISSETPROP x0 x1 x2 x3))))) (IMPLIES (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (NOT (_NISSETPROP x0 x2 x1 x4))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (NOT (_NISSETPROP x0 x2 x1 x5)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_T4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (NOT (_SET x0 x1 x2)) (IMPLIES (NOT (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (_NISSETPROP x0 x1 x2 x3))))) (IMPLIES (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (NOT (_NISSETPROP x0 x2 x1 x4))))) (_SET x0 x1 x2)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_T5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (NOT (_SET x0 x1 x2)) (IMPLIES (NOT (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (_NISSETPROP x0 x1 x2 x3))))) (EXISTS (LAM (x4 Z) (AND (x0 x4 x4) (_NISSETPROP x0 x2 x1 x4)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ISSET_TH5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z)) (IMPLIES (_SET x0 x1 x1) (FORALL (LAM (x2 (O Z)) (IMPLIES (_SET x0 x2 x2) (IMPLIES (NOT (_SET x0 x1 x2)) (OR (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (_NISSETPROP x0 x1 x2 x3)))) (EXISTS (LAM (x4 Z) (AND (x0 x4 x4) (_NISSETPROP x0 x2 x1 x4)))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UNMORE (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Y Y)) (LAM (x2 (O Z Y)) (_SETOF x0 (LAM (x3 Z) (EXISTS (LAM (x4 Y) (AND (x1 x4 x4) (_ESTI x0 x3 (x2 x4)))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UNMORE_DEC (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O Z Y)) (FORALL (LAM (x3 (O Z Y)) (IMPLIES (FORALL (LAM (x4 Y) (FORALL (LAM (x5 Y) (IMPLIES (x1 x4 x5) (_SET x0 (x2 x4) (x3 x5))))))) (_SET x0 (_UNMORE x0 x1 x2) (_UNMORE x0 x1 x3)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _EUNMORE1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O Z Y)) (IMPLIES (FORALL (LAM (x3 Y) (FORALL (LAM (x4 Y) (IMPLIES (x1 x3 x4) (_SET x0 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Y) (IMPLIES (x1 x6 x6) (IMPLIES (_ESTI x0 x5 (x2 x6)) (_ESTI x0 x5 (_UNMORE x0 x1 x2))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _UNMOREAPP (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES (PER x1) (FORALL (LAM (x2 (O Z Y)) (IMPLIES (FORALL (LAM (x3 Y) (FORALL (LAM (x4 Y) (IMPLIES (x1 x3 x4) (_SET x0 (x2 x3) (x2 x4))))))) (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (IMPLIES (_ESTI x0 x5 (_UNMORE x0 x1 x2)) (FORALL (LAM (x6 O) (IMPLIES (FORALL (LAM (x7 Y) (IMPLIES (x1 x7 x7) (IMPLIES (_ESTI x0 x5 (x2 x7)) x6)))) x6))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _REFR (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (x1 x8 x8))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _SYMR (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (x1 x8 x9) (x1 x9 x8))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TRR (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (x1 x8 x9) (IMPLIES (x1 x9 x10) (x1 x8 x10))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TR1R (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (x1 x10 x8) (IMPLIES (x1 x10 x9) (x1 x8 x9))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _TR2R (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (x1 x8 x10) (IMPLIES (x1 x9 x10) (x1 x8 x9))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECELT (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z Z)) (LAM (x2 Z) (_SETOF x0 (x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECELT_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (FORALL (LAM (x9 Z) (IMPLIES (x0 x8 x9) (_SET x0 (_ECELT x0 x1 x8) (_ECELT x0 x1 x9))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (_ESTI x0 x8 (_ECELT x0 x1 x8)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (x1 x8 x9) (_ESTI x0 x9 (_ECELT x0 x1 x8)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 (_ECELT x0 x1 x8)) (x1 x8 x9))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (x1 x8 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 (_ECELT x0 x1 x8)) (_ESTI x0 x10 (_ECELT x0 x1 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_TH4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (x1 x8 x9) (_SET x0 (_ECELT x0 x1 x8) (_ECELT x0 x1 x9)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (NOT (x1 x8 x9)) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 (_ECELT x0 x1 x8)) (NOT (_ESTI x0 x10 (_ECELT x0 x1 x9))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_TH5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (NOT (x1 x8 x9)) (_DISJ x0 (_ECELT x0 x1 x8) (_ECELT x0 x1 x9)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _1_TH6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (_NONEMPTY x0 (_ECELT x0 x1 x8)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECP (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z Z)) (LAM (x2 (O Z)) (LAM (x3 Z) (_SET x0 x2 (_ECELT x0 x1 x3)))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ANEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z Z)) (LAM (x2 (O Z)) (EXISTS (LAM (x3 Z) (AND (x0 x3 x3) (_ECP x0 x1 x2 x3))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (_ANEC x0 x1 (_ECELT x0 x1 x8)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 x8) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ECP x0 x1 x8 x10) (_ESTI x0 x9 (_ECELT x0 x1 x10))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 x8) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ECP x0 x1 x8 x10) (_SET x0 (_ECELT x0 x1 x10) (_ECELT x0 x1 x9))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_T3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 x8) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ECP x0 x1 x8 x10) (_SET x0 x8 (_ECELT x0 x1 x9))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 x8) (_SET x0 x8 (_ECELT x0 x1 x9))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 x8) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 x8) (x1 x9 x10)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_TH4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 x8) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (x1 x9 x10) (_ESTI x0 x10 x8)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_T4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ECP x0 x1 x8 x9) (_NONEMPTY x0 x8)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _2_TH5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (_NONEMPTY x0 x8)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_SET x0 x9 x9) (IMPLIES (_ANEC x0 x1 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 x8) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (_ESTI x0 x11 x9) (IMPLIES (x1 x10 x11) (_SET x0 x8 x9))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3_T1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_SET x0 x9 x9) (IMPLIES (_ANEC x0 x1 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 x8) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (_ESTI x0 x11 x9) (IMPLIES (NOT (x1 x10 x11)) (_DISJ x0 x8 (_ECELT x0 x1 x11)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_SET x0 x9 x9) (IMPLIES (_ANEC x0 x1 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 x8) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (_ESTI x0 x11 x9) (IMPLIES (NOT (x1 x10 x11)) (_DISJ x0 x8 x9))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3_T2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_SET x0 x9 x9) (IMPLIES (_SET x0 x8 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 x8) (_ESTI x0 x10 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3_T3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_SET x0 x9 x9) (IMPLIES (_SET x0 x8 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 x8) (NOT (_DISJ x0 x8 x9))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _3_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_SET x0 x8 x8) (IMPLIES (_ANEC x0 x1 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_SET x0 x9 x9) (IMPLIES (_SET x0 x8 x9) (NOT (_DISJ x0 x8 x9))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECT (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z Z)) (_OT (_SET x0) (_ANEC x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECT_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (PER (_ECT x0 x1)))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECTSET (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z Z)) (_OUT (_SET x0) (_ANEC x0 x1)))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECTSET_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (FORALL (LAM (x9 (O Z)) (IMPLIES (_SET x0 x8 x9) (IMPLIES (_ANEC x0 x1 x8) (_ECT x0 x1 (_ECTSET x0 x1 x8) (_ECTSET x0 x1 x9)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECTELT (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z Z)) (LAM (x2 Z) (_ECTSET x0 x1 (_ECELT x0 x1 x2))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECTELT_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (FORALL (LAM (x9 Z) (IMPLIES (x0 x8 x9) (_ECT x0 x1 (_ECTELT x0 x1 x8) (_ECTELT x0 x1 x9))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECECT (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (x0 (O Z Z)) (LAM (x1 (O Z Z)) (LAM (x2 (O Z)) x2))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _ECECT_DEC (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (FORALL (LAM (x9 (O Z)) (IMPLIES (_ECT x0 x1 x8 x9) (_SET x0 (_ECECT x0 x1 x8) (_ECECT x0 x1 x9))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (_ANEC x0 x1 (_ECECT x0 x1 x8)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (_NONEMPTY x0 (_ECECT x0 x1 x8)))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 O) (IMPLIES (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 (_ECECT x0 x1 x8)) x9)))) x9))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (_SET x0 (_ECELT x0 x1 x8) (_ECECT x0 x1 (_ECTELT x0 x1 x8))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (_ESTI x0 x8 (_ECECT x0 x1 (_ECTELT x0 x1 x8))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (_ESTI x0 x8 (_UNMORE x0 (_ECT x0 x1) (_ECECT x0 x1))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH7 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 (_ECECT x0 x1 x8)) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 (_ECECT x0 x1 x8)) (x1 x9 x10))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _4_TH8 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (_ESTI x0 x9 (_ECECT x0 x1 x8)) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (x1 x9 x10) (_ESTI x0 x10 (_ECECT x0 x1 x8)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5_TH1 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_ECT x0 x1 x9 x9) (IMPLIES (_ECT x0 x1 x8 x9) (_SET x0 (_ECECT x0 x1 x8) (_ECECT x0 x1 x9)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5_TH2 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_ECT x0 x1 x9 x9) (IMPLIES (_SET x0 (_ECECT x0 x1 x8) (_ECECT x0 x1 x9)) (_ECT x0 x1 x8 x9))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5_TH3 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_ECT x0 x1 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 (_ECECT x0 x1 x8)) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (_ESTI x0 x11 (_ECECT x0 x1 x9)) (IMPLIES (x1 x10 x11) (_ECT x0 x1 x8 x9))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5_TH4 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_ECT x0 x1 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 (_ECECT x0 x1 x8)) (IMPLIES (_ECT x0 x1 x8 x9) (_ESTI x0 x10 (_ECECT x0 x1 x9)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5_TH5 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z)) (IMPLIES (_ECT x0 x1 x8 x8) (FORALL (LAM (x9 (O Z)) (IMPLIES (_ECT x0 x1 x9 x9) (FORALL (LAM (x10 Z) (IMPLIES (x0 x10 x10) (IMPLIES (_ESTI x0 x10 (_ECECT x0 x1 x8)) (FORALL (LAM (x11 Z) (IMPLIES (x0 x11 x11) (IMPLIES (_ESTI x0 x11 (_ECECT x0 x1 x9)) (IMPLIES (_ECT x0 x1 x8 x9) (x1 x10 x11))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _5_TH6 (in landau0)
 (TYPE-VARIABLES Z)
 (definition (LAM (DUMMY Z) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 Z) (IMPLIES (x0 x8 x8) (FORALL (LAM (x9 Z) (IMPLIES (x0 x9 x9) (IMPLIES (x1 x8 x9) (_ECT x0 x1 (_ECTELT x0 x1 x8) (_ECTELT x0 x1 x9)))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIXFU (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Y Y)) (LAM (x2 (O Z Z)) (LAM (x3 (Z Y)) (FORALL (LAM (x4 Y) (IMPLIES (x0 x4 x4) (FORALL (LAM (x5 Y) (IMPLIES (x0 x5 x5) (IMPLIES (x1 x4 x5) (x2 (x3 x4) (x3 x5))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Z) (IMPLIES (x0 x13 x13) (IMPLIES (_ESTI x0 x13 (_ECECT x0 x1 x12)) (AND (_ESTI x0 x13 (_ECECT x0 x1 x12)) (x8 (x9 x13) (x9 x13)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Z) (IMPLIES (x0 x13 x13) (IMPLIES (_ESTI x0 x13 (_ECECT x0 x1 x12)) (EXISTS (LAM (x14 Z) (AND (x0 x14 x14) (AND (_ESTI x0 x14 (_ECECT x0 x1 x12)) (x8 (x9 x14) (x9 x13))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T3 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Z) (IMPLIES (x0 x13 x13) (IMPLIES (_ESTI x0 x13 (_ECECT x0 x1 x12)) (EXISTS (LAM (x14 Y) (AND (x8 x14 x14) (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x14))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T4 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (EXISTS (LAM (x13 Y) (AND (x8 x13 x13) (EXISTS (LAM (x14 Z) (AND (x0 x14 x14) (AND (_ESTI x0 x14 (_ECECT x0 x1 x12)) (x8 (x9 x14) x13))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T5 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (AND (_ESTI x0 x17 (_ECECT x0 x1 x12)) (x8 (x9 x17) x13)) (FORALL (LAM (x18 Z) (IMPLIES (x0 x18 x18) (IMPLIES (AND (_ESTI x0 x18 (_ECECT x0 x1 x12)) (x8 (x9 x18) x14)) (_ESTI x0 x17 (_ECECT x0 x1 x12))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T6 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (AND (_ESTI x0 x17 (_ECECT x0 x1 x12)) (x8 (x9 x17) x13)) (FORALL (LAM (x18 Z) (IMPLIES (x0 x18 x18) (IMPLIES (AND (_ESTI x0 x18 (_ECECT x0 x1 x12)) (x8 (x9 x18) x14)) (_ESTI x0 x18 (_ECECT x0 x1 x12))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T7 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (AND (_ESTI x0 x17 (_ECECT x0 x1 x12)) (x8 (x9 x17) x13)) (FORALL (LAM (x18 Z) (IMPLIES (x0 x18 x18) (IMPLIES (AND (_ESTI x0 x18 (_ECECT x0 x1 x12)) (x8 (x9 x18) x14)) (x1 x17 x18)))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T8 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (AND (_ESTI x0 x17 (_ECECT x0 x1 x12)) (x8 (x9 x17) x13)) (FORALL (LAM (x18 Z) (IMPLIES (x0 x18 x18) (IMPLIES (AND (_ESTI x0 x18 (_ECECT x0 x1 x12)) (x8 (x9 x18) x14)) (x8 (x9 x17) x13)))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T9 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (AND (_ESTI x0 x17 (_ECECT x0 x1 x12)) (x8 (x9 x17) x13)) (FORALL (LAM (x18 Z) (IMPLIES (x0 x18 x18) (IMPLIES (AND (_ESTI x0 x18 (_ECECT x0 x1 x12)) (x8 (x9 x18) x14)) (x8 (x9 x18) x14)))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T10 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (AND (_ESTI x0 x17 (_ECECT x0 x1 x12)) (x8 (x9 x17) x13)) (FORALL (LAM (x18 Z) (IMPLIES (x0 x18 x18) (IMPLIES (AND (_ESTI x0 x18 (_ECECT x0 x1 x12)) (x8 (x9 x18) x14)) (x8 x13 x14)))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T11 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (AND (_ESTI x0 x17 (_ECECT x0 x1 x12)) (x8 (x9 x17) x13)) (x8 x13 x14)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T12 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Y) (IMPLIES (x8 x13 x13) (FORALL (LAM (x14 Y) (IMPLIES (x8 x14 x14) (IMPLIES (EXISTS (LAM (x15 Z) (AND (x0 x15 x15) (AND (_ESTI x0 x15 (_ECECT x0 x1 x12)) (x8 (x9 x15) x13))))) (IMPLIES (EXISTS (LAM (x16 Z) (AND (x0 x16 x16) (AND (_ESTI x0 x16 (_ECECT x0 x1 x12)) (x8 (x9 x16) x14))))) (x8 x13 x14)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T13 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (_AMONE x8 (LAM (x13 Y) (EXISTS (LAM (x14 Z) (AND (x0 x14 x14) (AND (_ESTI x0 x14 (_ECECT x0 x1 x12)) (x8 (x9 x14) x13)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_T14 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (_E_ONE x8 (LAM (x13 Y) (EXISTS (LAM (x14 Z) (AND (x0 x14 x14) (AND (_ESTI x0 x14 (_ECECT x0 x1 x12)) (x8 (x9 x14) x13)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INDEQ (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Y Y)) (LAM (x2 (O Z Z)) (LAM (x3 (Z Y)) (LAM (x4 (O Y)) (THAT (LAM (x5 Z) (AND (x2 x5 x5) (EXISTS (LAM (x6 Y) (AND (x0 x6 x6) (AND (_ESTI x0 x6 (_ECECT x0 x1 x4)) (x2 (x3 x6) x5))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _INDEQ_DEC (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (DUMMY (Z Y)) (FORALL (LAM (x0 (O Y Y)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x2 Y) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Y) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Y) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Y) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Y) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Y) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z Z)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Z Y)) (FORALL (LAM (x10 (Z Y)) (IMPLIES (FORALL (LAM (x11 Y) (FORALL (LAM (x12 Y) (IMPLIES (x0 x11 x12) (x8 (x9 x11) (x10 x12))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x13 (O Y)) (FORALL (LAM (x14 (O Y)) (IMPLIES (_ECT x0 x1 x13 x14) (x8 (_INDEQ x0 x1 x8 x9 x13) (_INDEQ x0 x1 x8 x10 x14)))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_TH1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (EXISTS (LAM (x13 Z) (AND (x0 x13 x13) (AND (_ESTI x0 x13 (_ECECT x0 x1 x12)) (x8 (x9 x13) (_INDEQ x0 x1 x8 x9 x12))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_TH2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 (O Z)) (IMPLIES (_ECT x0 x1 x12 x12) (FORALL (LAM (x13 Z) (IMPLIES (x0 x13 x13) (IMPLIES (_ESTI x0 x13 (_ECECT x0 x1 x12)) (x8 (x9 x13) (_INDEQ x0 x1 x8 x9 x12))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _10_TH3 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (x8 (x9 x10) (x9 x11))))))) (IMPLIES (_FIXFU x0 x1 x8 x9) (FORALL (LAM (x12 Z) (IMPLIES (x0 x12 x12) (x8 (x9 x12) (_INDEQ x0 x1 x8 x9 (_ECTELT x0 x1 x12)))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _FIXFU2 (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Y Y)) (LAM (x2 (O Z Z)) (LAM (x3 (Z Y Y)) (FORALL (LAM (x4 Y) (IMPLIES (x0 x4 x4) (FORALL (LAM (x5 Y) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Y) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Y) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x4 x5) (IMPLIES (x1 x6 x7) (x2 (x3 x4 x6) (x3 x5 x7)))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 Z) (IMPLIES (x0 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (IMPLIES (x1 x14 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (x8 (x9 x14 x16) (x9 x15 x16)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 Z) (IMPLIES (x0 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (IMPLIES (x1 x14 x15) (FORALL (LAM (x16 Z) (FORALL (LAM (x17 Z) (IMPLIES (x0 x16 x17) (x8 (x9 x14 x16) (x9 x15 x17)))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _I (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (x0 (O Y Y)) (LAM (x1 (O Y Y)) (LAM (x2 (O Z Z)) (_INDEQ x0 x1 (LAM (x3 (Z Y)) (LAM (x4 (Z Y)) (FORALL (LAM (x5 Y) (FORALL (LAM (x6 Y) (IMPLIES (x0 x5 x6) (x2 (x3 x5) (x4 x6))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _I_DEC (in landau0)
 (TYPE-VARIABLES Z Y)
 (definition (LAM (DUMMY (Z Y)) (FORALL (LAM (x0 (O Y Y)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Y Y)) (IMPLIES TRUE (IMPLIES (FORALL (LAM (x2 Y) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Y) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Y) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Y) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Y) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Y) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Z Z)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Z Y Y)) (FORALL (LAM (x10 (Z Y Y)) (IMPLIES (FORALL (LAM (x11 Y) (FORALL (LAM (x12 Y) (IMPLIES (x0 x11 x12) (FORALL (LAM (x13 Y) (FORALL (LAM (x14 Y) (IMPLIES (x0 x13 x14) (x8 (x9 x11 x13) (x10 x12 x14)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x15 (O Y)) (FORALL (LAM (x16 (O Y)) (IMPLIES (_ECT x0 x1 x15 x16) (FORALL (LAM (x17 Y) (FORALL (LAM (x18 Y) (IMPLIES (x0 x17 x18) (x8 (_I x0 x1 x8 x9 x15 x17) (_I x0 x1 x8 x10 x16 x18))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T3 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (x1 x15 x16) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x14)) (FORALL (LAM (x18 Z) (FORALL (LAM (x19 Z) (IMPLIES (x0 x18 x19) (x8 (x9 x17 x18) (_I x0 x1 x8 x9 x14 x19))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T4 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (x1 x15 x16) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x14)) (x8 (x9 x17 x15) (_I x0 x1 x8 x9 x14 x15)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T5 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (x1 x15 x16) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x14)) (x8 (x9 x17 x16) (_I x0 x1 x8 x9 x14 x16)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T6 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (x1 x15 x16) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x14)) (x8 (x9 x17 x15) (x9 x17 x16)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T7 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (x1 x15 x16) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x14)) (x8 (_I x0 x1 x8 x9 x14 x15) (_I x0 x1 x8 x9 x14 x16)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T8 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (x1 x15 x16) (x8 (_I x0 x1 x8 x9 x14 x15) (_I x0 x1 x8 x9 x14 x16)))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T9 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 (O Z)) (IMPLIES (_ECT x0 x1 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (_ESTI x0 x16 (_ECECT x0 x1 x14)) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x15)) (x8 (_I x0 x1 x8 x9 x14 x17) (_INDEQ x0 x1 x8 (_I x0 x1 x8 x9 x14) x15)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T10 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 (O Z)) (IMPLIES (_ECT x0 x1 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (_ESTI x0 x16 (_ECECT x0 x1 x14)) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x15)) (FORALL (LAM (x18 Z) (FORALL (LAM (x19 Z) (IMPLIES (x0 x18 x19) (x8 (x9 x16 x18) (_I x0 x1 x8 x9 x14 x19))))))))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_T11 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 (O Z)) (IMPLIES (_ECT x0 x1 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (_ESTI x0 x16 (_ECECT x0 x1 x14)) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x15)) (x8 (x9 x16 x17) (_I x0 x1 x8 x9 x14 x17)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_TH1 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 (O Z)) (IMPLIES (_ECT x0 x1 x14 x14) (FORALL (LAM (x15 (O Z)) (IMPLIES (_ECT x0 x1 x15 x15) (FORALL (LAM (x16 Z) (IMPLIES (x0 x16 x16) (IMPLIES (_ESTI x0 x16 (_ECECT x0 x1 x14)) (FORALL (LAM (x17 Z) (IMPLIES (x0 x17 x17) (IMPLIES (_ESTI x0 x17 (_ECECT x0 x1 x15)) (x8 (x9 x16 x17) (_INDEQ x0 x1 x8 (_I x0 x1 x8 x9 x14) x15)))))))))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

(th~defdef _11_TH2 (in landau0)
 (TYPE-VARIABLES Y Z)
 (definition (LAM (DUMMY (Y Z)) (FORALL (LAM (x0 (O Z Z)) (IMPLIES (PER x0) (FORALL (LAM (x1 (O Z Z)) (IMPLIES (FORALL (LAM (x2 Z) (IMPLIES (x0 x2 x2) (x1 x2 x2)))) (IMPLIES (FORALL (LAM (x3 Z) (IMPLIES (x0 x3 x3) (FORALL (LAM (x4 Z) (IMPLIES (x0 x4 x4) (IMPLIES (x1 x3 x4) (x1 x4 x3)))))))) (IMPLIES (FORALL (LAM (x5 Z) (IMPLIES (x0 x5 x5) (FORALL (LAM (x6 Z) (IMPLIES (x0 x6 x6) (FORALL (LAM (x7 Z) (IMPLIES (x0 x7 x7) (IMPLIES (x1 x5 x6) (IMPLIES (x1 x6 x7) (x1 x5 x7)))))))))))) (FORALL (LAM (x8 (O Y Y)) (IMPLIES (PER x8) (FORALL (LAM (x9 (Y Z Z)) (IMPLIES (FORALL (LAM (x10 Z) (FORALL (LAM (x11 Z) (IMPLIES (x0 x10 x11) (FORALL (LAM (x12 Z) (FORALL (LAM (x13 Z) (IMPLIES (x0 x12 x13) (x8 (x9 x10 x12) (x9 x11 x13)))))))))))) (IMPLIES (_FIXFU2 x0 x1 x8 x9) (FORALL (LAM (x14 Z) (IMPLIES (x0 x14 x14) (FORALL (LAM (x15 Z) (IMPLIES (x0 x15 x15) (x8 (x9 x14 x15) (_INDEQ x0 x1 x8 (_I x0 x1 x8 x9 (_ECTELT x0 x1 x14)) (_ECTELT x0 x1 x15))))))))))))))))))))))))))
 (help "Defn from Jutting's translation of Landau's Grundlagen"))

