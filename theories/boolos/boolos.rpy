OMEGA-BASIC LEMMA (CONC) ((N ONE))
OMEGA-BASIC SUPPORT (L1) (NIL)
TACTICS DEFSI (L1) default default
OMEGA-BASIC SUPPORT (L2) (NIL)
EXTERN CALL-OTTER-ON-NODE (L2) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (CONC) ((FORALL
                           (LAM (Y I) (IMPLIES (N Y) (N (S Y))))))
OMEGA-BASIC SUPPORT (L3) (NIL)
TACTICS DEFSI (L3) default default
EXTERN CALL-OTTER-ON-NODE (L4) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (CONC) ((N (S (S (S (S ONE))))))
OMEGA-BASIC SUPPORT (L5) (NIL)
TACTICS DEFSI (L5) default default
EXTERN CALL-OTTER-ON-NODE (L6) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (CONC) ((E ONE))
OMEGA-BASIC SUPPORT (L7) ((A4))
TACTICS DEFSI (L7) (NIL) default
EXTERN CALL-OTTER-ON-NODE (L8) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (CONC) ((FORALL
                           (LAM (Y I) (IMPLIES (E Y) (E (S Y))))))
OMEGA-BASIC SUPPORT (L9) ((L2 L4 L6 L8 A4))
TACTICS DEFSI (L9) (NIL) default
OMEGA-BASIC SUPPORT (L10) ((A5))
EXTERN CALL-OTTER-ON-NODE (L10) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (CONC) ((E (S ONE)))
OMEGA-BASIC SUPPORT (L11) ((L7 L9))
EXTERN CALL-OTTER-ON-NODE (L11) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (CONC) ((FORALL
                           (LAM (NN I)
                            (IMPLIES (N NN)
                             (FORALL
                              (LAM (X I)
                               (IMPLIES (N X) (E (F NN X)))))))))
OMEGA-BASIC LEMMA (L12) ((M ONE))
RULES DEFN-CONTRACT default default default
OMEGA-BASIC LEMMA default ((FORALL (LAM (X I) (IMPLIES (N X) (Q X)))))
OMEGA-BASIC LEMMA default ((Q ONE))
RULES DEFN-CONTRACT default default default
OMEGA-BASIC SUPPORT default ((A1 L11))
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (L15) ((FORALL (LAM (X I) (IMPLIES (Q X) (Q (S X))))))
TACTICS DEFN-CONTRACT* default default default
OMEGA-BASIC SUPPORT (L19) ((A2 L9))
EXTERN CALL-OTTER-ON-NODE (L19) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC SUPPORT (L15) ((L16 L18))
RULES DEFN-CONTRACT (L15) default default
EXTERN CALL-OTTER-ON-NODE (L20) default default (TEST) default default default default default default default default default default default
OMEGA-BASIC SUPPORT (L14) ((L15))
RULES DEFN-EXPAND (L15) (Q) default
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA default ((FORALL
                            (LAM (N I) (IMPLIES (M N) (M (S N))))))
TACTICS DEFN-CONTRACT* default default default
RULES FORALLI (L23) (n1) default
RULES IMPI default
OMEGA-BASIC LOCAL-DEF-INTRO ((LAM (X I) (E (F (S n1) X))))
OMEGA-BASIC LEMMA (L26) ((LD1 ONE))
RULES DEFN-CONTRACT-LOCAL-DEF (L27) (NIL) (LD1) ((0))
OMEGA-BASIC SUPPORT default ((A1 L11))
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (L26) ((FORALL
                          (LAM (X I) (IMPLIES (LD1 X) (LD1 (S X))))))
RULES FORALLI default (N2) default
RULES IMPI default
RULES DEFN-EXPAND-LOCAL-DEF (L31) (NIL) (LD1) ((0))
RULES DEFN-EXPAND-LOCAL-DEF (NIL) (L31) (LD1) ((0))
RULES DEFN-EXPAND (L33) default default
OMEGA-BASIC SUPPORT (L32) ((L34 L25 A3))
RULES DEFN-CONTRACT-LOCAL-DEF (L32) (NIL) (LD1) ((0))
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (L12) ((FORALL (LAM (NN I) (IMPLIES (N NN) (M NN)))))
RULES DEFN-CONTRACT default default default
OMEGA-BASIC SUPPORT default ((L13 L22))
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC SUPPORT (L12) ((L36))
RULES DEFN-EXPAND (L36) (M) default
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA (CONC) ((E (F (S (S (S (S ONE)))) (S (S (S (S ONE)))))))
OMEGA-BASIC SUPPORT (L39) ((L5 L12))
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC SUPPORT (CONC) ((L39))
RULES DEFN-EXPAND (L39) default default
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
OMEGA-BASIC LEMMA default ((FORALL (LAM (X I) (IMPLIES (N X) (LD1 X)))))
RULES DEFN-CONTRACT (L41) default default
OMEGA-BASIC SUPPORT (L42) ((L29 L27))
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
RULES DEFN-EXPAND-LOCAL-DEF (NIL) (L41) (LD1) ((1 0 2 0))
OMEGA-BASIC SUPPORT default ((L43))
EXTERN CALL-OTTER-ON-NODE default default default (TEST) default default default default default default default default default default default
