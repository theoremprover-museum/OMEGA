(in-package :tag)

(ieda 'N "wolf" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))

(ieda 'N "fox" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))

(ieda 'N "bird" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))

(ieda 'N "caterpillar" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))

(ieda 'N "snail" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))

(ieda 'N "animal" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))

(ieda 'N "grain" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))

(ieda 'N "plant" '("" N-SPECIFIER-MODIFIER 
                      (OBL (FUNC SPECIFIER) (real BOTH))
                      (OBL (FUNC N) (person 3) (cat N))
                      (fac (FUNC modifier))))


(ieda 'V "eat" %e-subj-accobj-net%)
(setq $english-inflection-data$
      (append $english-inflection-data$ '(("eat"
	 (indikativ (aktiv (praesens (sg (1 . "eat") (2 . "eat") (3 . "eats"))
				     (pl (1 . "eat") (2 . "eat") (3 . "eat")))))))))

(ieda 'adj "small" '("" adj))
