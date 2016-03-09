OMEGA-BASIC EXPAND-NODE (l18)
OMEGA-BASIC LEMMA default ((INT 2))
TACTICS WELLSORTED (l25) default
EXTERN CALL-OTTER-ON-NODE (l18) default default default default default default default default default default default default default default

OMEGA-BASIC EXPAND-NODE (l13)
TACTICS BY-COMPUTATION (L13) ((l11))
	
OMEGA-BASIC EXPAND-NODE (l21)
TACTICS BY-COMPUTATION (L21) ((l20 l13))

OMEGA-BASIC EXPAND-NODE (l15)
OMEGA-BASIC EXPAND-NODE (l23)
MBASE IMPORT-ASS (SQUARE-EVEN)
EXTERN CALL-OTTER-ON-NODE (l15) default default default default default default default default default default default default default default
EXTERN CALL-OTTER-ON-NODE (l23) default default default default default default default default default default default default default default

OMEGA-BASIC EXPAND-NODE (l14)
RULES DEFN-CONTRACT (L14) default default
OMEGA-BASIC LEMMA (l40) ((INT (POWER N 2)))
EXTERN CALL-OTTER-ON-NODE (l40) default default default default default default default default default default default default default default

OMEGA-BASIC EXPAND-NODE (l22)
RULES DEFN-CONTRACT (L22) default default
OMEGA-BASIC LEMMA (l50) ((INT (POWER K 2)))
EXTERN CALL-OTTER-ON-NODE (l50) default default default default default default default default default default default default default default

OMEGA-BASIC ADD-SUPPORT (l51) (l19)
TACTICS WELLSORTED (l51) ((l19))
OMEGA-BASIC ADD-SUPPORT (l41) (l6)
TACTICS WELLSORTED (l41) ((l6))

OMEGA-BASIC EXPAND-NODE (l24)
RULES DEFN-CONTRACT (L24) default default
TACTICS DEFN-CONTRACT* (L59) (divisor) default
RULES DEFN-EXPAND (L15) default default
RULES DEFN-EXPAND (L23) default default
OMEGA-BASIC LEMMA (l60) ((not (= 1 2)))
OMEGA-BASIC LEMMA (l60) ((Int 2))
EXTERN CALL-OTTER-ON-NODE (l60) default default default default default default default default default default default default default default

TACTICS WELLSORTED (l64) default
OMEGA-BASIC LEMMA (l63) ((not (= (s zero) (s (s zero)))))
MBASE IMPORT-ASS (NAT-INJ-SUCC)
MBASE IMPORT-ASS (SUCC-NAT)
MBASE IMPORT-ASS (ZERO-NAT)
MBASE IMPORT-ASS (NAT-NO-PRED-ZERO)
RULES DEFN-EXPAND (NAT-INJ-SUCC) default default
EXTERN CALL-OTTER-ON-NODE (l102) default default default default default default default default default default default default default default
