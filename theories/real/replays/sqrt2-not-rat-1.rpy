DECLARATION DECLARE ((CONSTANTS (M NUM) (N NUM) (K NUM)))
RULES NOTI default default
MBASE IMPORT-ASS (RAT-CRITERION)
TACTICS FORALLE-SORT default default ((SQRT 2)) default
TACTICS EXISTSE-SORT default default (N) default
TACTICS ANDE default default default
TACTICS EXISTSE-SORT (L7) default (M) default
TACTICS ANDE* (L8) (NIL)
OMEGA-BASIC LEMMA default ((= (POWER M 2) (TIMES 2 (POWER N 2))))
TACTICS BY-COMPUTATION (L13) ((L11))
OMEGA-BASIC LEMMA (L9) ((EVENP (POWER M 2)))
RULES DEFN-CONTRACT default default default
OMEGA-BASIC LEMMA (L9) ((INT (POWER N 2)))
TACTICS WELLSORTED default default
TACTICS EXISTSI-SORT (L15) ((POWER N 2)) (L13) (L16) default
MBASE IMPORT-ASS (SQUARE-EVEN)
TACTICS ASSERT ((EVENP M)) ((SQUARE-EVEN L10 L14)) (NIL)
RULES DEFN-EXPAND (L17) default default
TACTICS EXISTSE-SORT default default (K) default
TACTICS ANDE (L19) default default
OMEGA-BASIC LEMMA default ((= (POWER N 2) (TIMES 2 (POWER K 2))))
TACTICS BY-COMPUTATION (L23) ((L13 L22))
OMEGA-BASIC LEMMA default ((EVENP (POWER N 2)))
RULES DEFN-CONTRACT default default default
OMEGA-BASIC LEMMA (L20) ((INT (POWER K 2)))
TACTICS WELLSORTED (L26) ((L21))
TACTICS EXISTSI-SORT default ((POWER K 2)) (L23) default default
TACTICS ASSERT ((EVENP N)) ((SQUARE-EVEN L6 L24)) (NIL)
MBASE IMPORT-ASS (EVEN-COMMON-DIVISOR)
OMEGA-BASIC LEMMA (L20) ((INT 2))
TACTICS WELLSORTED (L28) (NIL)
TACTICS ASSERT (FALSE) ((EVEN-COMMON-DIVISOR L10 L6 L12 L17 L27 L28)) (NIL)
RULES WEAKEN default default