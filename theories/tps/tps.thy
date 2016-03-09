;;; -*- syntax: common-lisp; package: KEIM; base: 10; mode: Theory -*-
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;;                                                                          ;;
;;   Copyright (C) 1996 by AG Siekmann, Fachbereich Informatik,             ;;
;;   Universitaet des Saarlandes, Saarbruecken, Germany.                    ;;
;;   All rights reserved.                                                   ;;
;;   For information about this program, write to:                          ;;
;;     OMEGA Project                                                         ;;
;;     AG Siekmann/FB Informatik                                            ;;
;;     Universitaet des Saarlandes                                          ;;
;;     Postfach 1150                                                        ;;
;;     D-66041 Saarbruecken                                                 ;;
;;     Germany                                                              ;;
;;   electronic mail: keim@cs.uni-sb.de                                     ;;
;;                                                                          ;;
;;   The author makes no representations about the suitability of this      ;;
;;   software for any purpose.  It is provided "AS IS" without express or   ;;
;;   implied warranty.  In particular, it must be understood that this      ;;
;;   software is an experimental version, and is not suitable for use in    ;;
;;   any safety-critical application, and the author denies a license for   ;;
;;   such use.                                                              ;;
;;                                                                          ;;
;;   You may use, copy, modify and distribute this software for any         ;;
;;   noncommercial and non-safety-critical purpose.  Use of this software   ;;
;;   in a commercial product is not included under this license.  You must  ;;
;;   maintain this copyright statement in all copies of this software that  ;;
;;   you modify or distribute.                                              ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;

(in-package :omega)

(th~deftheory tps
              (uses base)
	      (help "TPS theory"))

; cebrown 13/8/01, added a few abbrevs and thms from the TPS library

; Standard Abbreviations Built Into TPS
(th~defdef SUBSET
           (in tps)
           (type-variables a)
           (definition
             (lam (P (o a))
                  (lam (R (o a))
                       (forall (lam (|x| a)
                          (implies (P |x|) (R |x|)))))))
           )

(th~defdef IN-IMAGE
           (in tps)
           (type-variables b a)
           (definition
             (lam (|f| (a b))
                  (lam (|x| (o b))
                       (lam (|z| a)
                            (exists (lam (|t| b)
                                         (and (|x| |t|) (= |z| (|f| |t|)))))))))
)

(th~defdef COMPLEMENT-TPS
 (in tps)
 (type-variables a)
 (definition
(lam (S (o a)) (lam (|x| a) (not (S |x|)))))
)

(th~defdef EQUIVS
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (lam (R (o a)) (forall (lam (|x| a) (equiv (P |x|) (R |x|)))))))
)

(th~defdef INTERSECT
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (lam (R (o a)) (lam (|x| a) (and (P |x|) (R |x|))))))
)

(th~defdef POWERSET
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (lam (R (o a)) (SUBSET R P))))
)

(th~defdef SETEQUIV
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (lam (R (o a)) (and (SUBSET P R) (SUBSET R P)))))
)

(th~defdef SETINTERSECT
 (in tps)
 (type-variables a)
 (definition
(lam (D (o (o a))) (lam (|x| a) (forall (lam (S (o a)) (implies (D S) (S |x|)))))))
)

(th~defdef SETUNION
 (in tps)
 (type-variables a)
 (definition
(lam (D (o (o a))) (lam (|x| a) (exists (lam (S (o a)) (and (D S) (S |x|)))))))
)

(th~defdef UNION
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (lam (R (o a)) (lam (|z| a) (or (P |z|) (R |z|))))))
)

(th~defdef UNITSET
 (in tps)
 (type-variables a)
 (definition
(lam (|x| a) (lam (|y| a) (= |x| |y|))))
)

(th~defdef SIGMA1
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (exists (lam (|y| a) (= P (= |y|))))))
)

(th~defdef COND
 (in tps)
 (type-variables c)
 (definition
(lam (|x| c) (lam (|y| c) (lam (|p| o) (that (lam (|q| c) (or (and |p| (= |x| |q|)) (and (not |p|) (= |y| |q|)))))))))
)

(th~defdef EQP
 (in tps)
 (type-variables a b)
 (definition
(lam (|p| (o b)) (lam (|q| (o a)) (exists (lam (|s| (a b)) (and (forall (lam (|x| b) (implies (|p| |x|) (|q| (|s| |x|))))) (forall (lam (|y| a) (implies (|q| |y|) (SIGMA1 (lam (|x| b) (and (|p| |x|) (= |y| (|s| |x|))))))))))))))
)

(th~defdef NC
 (in tps)
 (type-variables b)
 (definition
(lam (|u| (o (o b))) (exists (lam (|p| (o b)) (= |u| (EQP |p|))))))
)

(th~defdef ZERO
 (in tps)
 (definition
(lam (|p| (o i)) (not (exists (lam (|x| i) (|p| |x|))))))
)

(th~defdef SUCC
 (in tps)
 (definition
(lam (|n| (o (o i))) (lam (|p| (o i)) (exists (lam (|x| i) (and (|p| |x|) (|n| (lam (|t| i) (and (not (= |t| |x|)) (|p| |t|))))))))))
)

(th~defdef ONE
 (in tps)
 (definition
(SUCC ZERO))
)

(th~defdef NAT
 (in tps)
 (definition
(lam (|n| (o (o i))) (forall (lam (|p| (o (o (o i)))) (implies (and (|p| ZERO) (forall (lam (|x| (o (o i))) (implies (|p| |x|) (|p| (SUCC |x|)))))) (|p| |n|))))))
)

(th~defdef FINITE
 (in tps)
 (definition
(lam (|p| (o i)) (exists (lam (|n| (o (o i))) (and (NAT |n|) (|n| |p|))))))
)

(th~defdef LESS=
 (in tps)
 (definition
(lam (|x| (o (o i))) (lam (|y| (o (o i))) (forall (lam (|p| (o (o (o i)))) (implies (and (|p| |x|) (forall (lam (|z| (o (o i))) (implies (|p| |z|) (|p| (SUCC |z|)))))) (|p| |y|)))))))
)

(th~defdef MU
 (in tps)
 (definition
(lam (|p| (o (o (o i)))) (that (lam (|x| (o (o i))) (and (and (NAT |x|) (|p| |x|)) (forall (lam (|y| (o (o i))) (implies (NAT |y|) (implies (|p| |y|) (LESS= |x| |y|))))))))))
)

(th~defdef RECURSION
 (in tps)
 (definition
(lam (|h| ((o (o i)) (o (o i)) (o (o i)))) (lam (|g| (o (o i))) (lam (|n| (o (o i))) (that (lam (|m| (o (o i))) (forall (lam (|w| (o (o (o i)) (o (o i)))) (implies (and (|w| ZERO |g|) (forall (lam (|x| (o (o i))) (forall (lam (|y| (o (o i))) (implies (|w| |x| |y|) (|w| (SUCC |x|) (|h| |x| |y|)))))))) (|w| |n| |m|))))))))))
)

; Standard Wffs Built Into TPS
(th~deftheorem X2106
 (in tps)
 (conclusion 
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (forall (lam (R (o i)) (implies (and (forall (lam (|x| i) (implies (R |x|) (P |x|)))) (forall (lam (|x| i) (implies (not (Q |x|)) (R |x|))))) (forall (lam (|x| i) (or (P |x|) (Q |x|))))))))))))
)

(th~deftheorem X2107
 (in tps)
 (conclusion 
 (forall (lam (Q (o i i)) (forall (lam (|b| i) (forall (lam (|a| i) (forall (lam (R (o i i)) (implies (and (and (R |a| |b|) (forall (lam (|x| i) (forall (lam (|y| i) (implies (R |x| |y|) (and (R |y| |x|) (Q |x| |y|)))))))) (forall (lam (|u| i) (forall (lam (|v| i) (implies (Q |u| |v|) (Q |u| |u|))))))) (and (Q |a| |a|) (Q |b| |b|))))))))))))
)

(th~deftheorem X2108
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (forall (lam (|x| i) (exists (lam (|y| i) (implies (P |x|) (P |y|)))))))))
)

(th~deftheorem X2109
 (in tps)
 (conclusion 
 (forall (lam (Q (o i)) (forall (lam (|p| o) (equiv (exists (lam (|x| i) (and |p| (Q |x|)))) (and |p| (exists (lam (|x| i) (Q |x|))))))))))
)

(th~deftheorem X2110
 (in tps)
 (conclusion 
 (forall (lam (Q (o i i)) (forall (lam (R (o i)) (implies (and (and (exists (lam (|x| i) (R |x|))) (forall (lam (|y| i) (implies (R |y|) (exists (lam (|z| i) (Q |y| |z|))))))) (forall (lam (|x| i) (forall (lam (|y| i) (implies (Q |x| |y|) (Q |x| |x|))))))) (exists (lam (|x| i) (exists (lam (|y| i) (and (Q |x| |y|) (R |y|))))))))))))
)

(th~deftheorem X2111
 (in tps)
 (conclusion 
 (forall (lam (Q (o i i)) (forall (lam (P (o i i)) (implies (and (forall (lam (|x| i) (implies (exists (lam (|y| i) (P |x| |y|))) (forall (lam (|y| i) (Q |x| |y|)))))) (forall (lam (|z| i) (exists (lam (|y| i) (P |z| |y|)))))) (forall (lam (|y| i) (forall (lam (|x| i) (Q |x| |y|)))))))))))
)

(th~deftheorem X2112
 (in tps)
 (conclusion 
 (forall (lam (P (o i i)) (forall (lam (Q (o i i)) (forall (lam (S (o i)) (implies (and (and (exists (lam (|v| i) (forall (lam (|x| i) (P |x| |v|))))) (forall (lam (|x| i) (implies (S |x|) (exists (lam (|y| i) (Q |y| |x|))))))) (forall (lam (|x| i) (forall (lam (|y| i) (implies (P |x| |y|) (not (Q |x| |y|)))))))) (exists (lam (|u| i) (not (S |u|))))))))))))
)

(th~deftheorem X2113
 (in tps)
 (conclusion 
 (forall (lam (R (o i i)) (forall (lam (P (o i)) (implies (and (forall (lam (|y| i) (exists (lam (|w| i) (R |y| |w|))))) (exists (lam (|z| i) (forall (lam (|x| i) (implies (P |x|) (not (R |z| |x|)))))))) (exists (lam (|x| i) (not (P |x|))))))))))
)

(th~deftheorem X2114
 (in tps)
 (conclusion 
 (forall (lam (|a| i) (forall (lam (|b| i) (forall (lam (R (o i i)) (implies (and (forall (lam (|x| i) (R |x| |b|))) (forall (lam (|y| i) (implies (exists (lam (|z| i) (R |y| |z|))) (R |a| |y|))))) (exists (lam (|u| i) (forall (lam (|v| i) (R |u| |v|)))))))))))))
)

(th~deftheorem X2115
 (in tps)
 (conclusion 
 (forall (lam (|g| (i i)) (forall (lam (P (o i i)) (forall (lam (M (o i)) (forall (lam (|f| (i i i)) (forall (lam (Q (o i)) (implies (and (and (forall (lam (|x| i) (implies (exists (lam (|y| i) (P |x| |y|))) (forall (lam (|z| i) (P |z| |z|)))))) (forall (lam (|u| i) (exists (lam (|v| i) (or (P |u| |v|) (and (M |u|) (Q (|f| |u| |v|))))))))) (forall (lam (|w| i) (implies (Q |w|) (not (M (|g| |w|))))))) (forall (lam (|u| i) (exists (lam (|v| i) (and (P (|g| |u|) |v|) (P |u| |u|))))))))))))))))))
)

(th~deftheorem X2116
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (forall (lam (R (o i i)) (forall (lam (|h| (i i)) (forall (lam (|g| (i i)) (implies (and (forall (lam (|x| i) (exists (lam (|y| i) (implies (P |x|) (and (R |x| (|g| (|h| |y|))) (P |y|))))))) (forall (lam (|w| i) (implies (P |w|) (and (P (|g| |w|)) (P (|h| |w|))))))) (forall (lam (|x| i) (implies (P |x|) (exists (lam (|y| i) (and (R |x| |y|) (P |y|)))))))))))))))))
)

(th~deftheorem X2117
 (in tps)
 (conclusion 
 (forall (lam (R (o i i)) (implies (and (forall (lam (|u| i) (forall (lam (|v| i) (equiv (R |u| |u|) (R |u| |v|)))))) (forall (lam (|w| i) (forall (lam (|z| i) (equiv (R |w| |w|) (R |z| |w|))))))) (implies (exists (lam (|x| i) (R |x| |x|))) (forall (lam (|y| i) (R |y| |y|))))))))
)

(th~deftheorem X2118
 (in tps)
 (conclusion 
 (forall (lam (R (o i)) (forall (lam (Q (o i)) (forall (lam (|p| o) (implies (forall (lam (|x| i) (or (and |p| (Q |x|)) (and (not |p|) (R |x|))))) (or (forall (lam (|x| i) (Q |x|))) (forall (lam (|x| i) (R |x|))))))))))))
)

(th~deftheorem X2119
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (exists (lam (|y| i) (forall (lam (|x| i) (implies (P |y|) (P |x|)))))))))
)

(th~deftheorem X2120
 (in tps)
 (conclusion 
 (forall (lam (P (o i i)) (implies (forall (lam (|u| i) (forall (lam (|v| i) (forall (lam (|w| i) (or (P |u| |v|) (P |v| |w|)))))))) (exists (lam (|x| i) (forall (lam (|y| i) (P |x| |y|)))))))))
)

(th~deftheorem X2121
 (in tps)
 (conclusion 
 (forall (lam (|f| (i i)) (forall (lam (|h| (i i)) (forall (lam (|a| i) (forall (lam (P (o i i i)) (exists (lam (|v| i) (forall (lam (|y| i) (exists (lam (|z| i) (implies (or (P |a| |y| (|h| |y|)) (P |v| |y| (|f| |y|))) (P |v| |y| |z|)))))))))))))))))
)

(th~deftheorem X2122
 (in tps)
 (conclusion 
 (forall (lam (R (o i i)) (implies (implies (exists (lam (|x| i) (R |x| |x|))) (forall (lam (|y| i) (R |y| |y|)))) (exists (lam (|u| i) (forall (lam (|v| i) (implies (R |u| |u|) (R |v| |v|))))))))))
)

(th~deftheorem X2123
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (forall (lam (|x| i) (forall (lam (Q (o i)) (implies (exists (lam (|y| i) (implies (P |y|) (Q |x|)))) (exists (lam (|y| i) (implies (P |y|) (Q |y|))))))))))))
)

(th~deftheorem X2124
 (in tps)
 (conclusion 
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (|x| i) (implies (P |x|) (Q |x|)))) (implies (forall (lam (|x| i) (P |x|))) (exists (lam (|x| i) (Q |x|))))))))))
)

(th~deftheorem X2125
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (equiv (exists (lam (|x| i) (forall (lam (|y| i) (equiv (P |x|) (P |y|)))))) (equiv (exists (lam (|x| i) (P |x|))) (forall (lam (|y| i) (P |y|))))))))
)

(th~deftheorem X2126
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (equiv (forall (lam (|x| i) (equiv (P |x|) (exists (lam (|y| i) (P |y|)))))) (equiv (forall (lam (|x| i) (P |x|))) (exists (lam (|y| i) (P |y|))))))))
)

(th~deftheorem X2127
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (implies (exists (lam (|x| i) (forall (lam (|y| i) (equiv (P |y|) (P |x|)))))) (or (forall (lam (|x| i) (P |x|))) (forall (lam (|x| i) (not (P |x|)))))))))
)

(th~deftheorem X2128
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (equiv (forall (lam (|x| i) (equiv (P |x|) (forall (lam (|y| i) (P |y|)))))) (equiv (exists (lam (|x| i) (P |x|))) (forall (lam (|y| i) (P |y|))))))))
)

(th~deftheorem X2129
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (forall (lam (Q (o i)) (equiv (equiv (exists (lam (|x| i) (forall (lam (|y| i) (equiv (P |x|) (P |y|)))))) (equiv (exists (lam (|x| i) (Q |x|))) (forall (lam (|y| i) (P |y|))))) (equiv (exists (lam (|x| i) (forall (lam (|y| i) (equiv (Q |x|) (Q |y|)))))) (equiv (exists (lam (|x| i) (P |x|))) (forall (lam (|y| i) (Q |y|)))))))))))
)

(th~deftheorem X2130
 (in tps)
 (conclusion 
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (implies (forall (lam (|x| i) (P |x|))) (or (not (exists (lam (|y| i) (Q |y|)))) (exists (lam (|z| i) (implies (P |z|) (Q |z|)))))))))))
)

(th~deftheorem X2131
 (in tps)
 (conclusion 
 (forall (lam (Q (o i i i)) (forall (lam (P (o i)) (implies (forall (lam (|x| i) (P |x|))) (exists (lam (|y| i) (implies (forall (lam (|x| i) (forall (lam (|z| i) (Q |x| |y| |z|))))) (not (forall (lam (|z| i) (and (P |z|) (not (Q |y| |y| |z|)))))))))))))))
)

(th~deftheorem X2132
 (in tps)
 (conclusion 
 (forall (lam (Q (o i i)) (forall (lam (R (o i i)) (implies (forall (lam (|w| i) (not (R |w| |w|)))) (exists (lam (|x| i) (exists (lam (|y| i) (and (not (R |x| |y|)) (implies (Q |y| |x|) (forall (lam (|z| i) (Q |z| |z|)))))))))))))))
)

(th~deftheorem X2133
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (forall (lam (Q (o i i)) (implies (and (and (forall (lam (|x| i) (implies (exists (lam (|y| i) (Q |x| |y|))) (P |x|)))) (forall (lam (|v| i) (exists (lam (|u| i) (Q |u| |v|)))))) (forall (lam (|w| i) (forall (lam (|z| i) (implies (Q |w| |z|) (or (Q |z| |w|) (Q |z| |z|)))))))) (forall (lam (|z| i) (P |z|)))))))))
)

(th~deftheorem X2134
 (in tps)
 (conclusion 
 (forall (lam (Q (o i i)) (forall (lam (P (o i i)) (implies (forall (lam (|z| i) (exists (lam (|x| i) (or (forall (lam (|y| i) (P |x| |y|))) (Q |x| |z|)))))) (forall (lam (|y| i) (exists (lam (|x| i) (or (P |x| |y|) (Q |x| |y|))))))))))))
)

(th~deftheorem X2135
 (in tps)
 (conclusion 
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (exists (lam (|x| i) (forall (lam (|y| i) (implies (and (P |x|) (Q |y|)) (or (Q |x|) (P |y|))))))))))))
)

(th~deftheorem X2136
 (in tps)
 (conclusion 
 (forall (lam (|z| i) (forall (lam (P (o i i i)) (exists (lam (|x| i) (exists (lam (|y| i) (forall (lam (|u| i) (implies (P |x| |y| |z|) (P |u| |x| |x|)))))))))))))
)

(th~deftheorem X2137
 (in tps)
 (conclusion 
 (forall (lam (P (o i)) (forall (lam (Q (o i)) (exists (lam (|x| i) (forall (lam (|y| i) (implies (P |x|) (or (Q |x|) (P |y|))))))))))))
)

(th~deftheorem X2138
 (in tps)
 (conclusion 
 (forall (lam (F (o i i)) (forall (lam (D (o i i i)) (forall (lam (S (o i i)) (implies (and (and (forall (lam (|x| i) (exists (lam (|y| i) (F |x| |y|))))) (exists (lam (|x| i) (forall (lam (|e| i) (exists (lam (|n| i) (forall (lam (|w| i) (implies (S |n| |w|) (D |w| |x| |e|))))))))))) (forall (lam (|e| i) (exists (lam (|d| i) (forall (lam (|a| i) (forall (lam (|b| i) (implies (D |a| |b| |d|) (forall (lam (|y| i) (forall (lam (|z| i) (implies (and (F |a| |y|) (F |b| |z|)) (D |y| |z| |e|)))))))))))))))) (exists (lam (|y| i) (forall (lam (|e| i) (exists (lam (|m| i) (forall (lam (|w| i) (implies (S |m| |w|) (forall (lam (|z| i) (implies (F |w| |z|) (D |z| |y| |e|)))))))))))))))))))))
)

(th~deftheorem X5200
 (in tps)
 (conclusion 
  (all-types a (forall (lam (|y| (o a)) (forall (lam (|x| (o a)) (= (UNION |x| |y|) (SETUNION (lam (|v| (o a)) (or (= |v| |x|) (= |v| |y|)))))))))))
)

(th~deftheorem X5201
 (in tps)
 (conclusion 
  (all-types a (forall (lam (|y| (o a)) (forall (lam (|x| (o a)) (= (INTERSECT |x| |y|) (SETINTERSECT (lam (|v| (o a)) (or (= |v| |x|) (= |v| |y|)))))))))))
)

(th~deftheorem X5202
 (in tps)
 (conclusion 
  (all-types b a (forall (lam (|f| (a b)) (forall (lam (|y| (o b)) (forall (lam (|x| (o b)) (= (IN-IMAGE |f| (UNION |x| |y|)) (UNION (IN-IMAGE |f| |x|) (IN-IMAGE |f| |y|)))))))))))
)

(th~deftheorem X5203
 (in tps)
 (conclusion 
  (all-types b a (forall (lam (|f| (a b)) (forall (lam (|y| (o b)) (forall (lam (|x| (o b)) (SUBSET (IN-IMAGE |f| (INTERSECT |x| |y|)) (INTERSECT (IN-IMAGE |f| |x|) (IN-IMAGE |f| |y|)))))))))))
)

(th~deftheorem X5204
 (in tps)
 (conclusion 
  (all-types a b (forall (lam (|w| (o (o b))) (forall (lam (|f| (a b)) (= (IN-IMAGE |f| (SETUNION |w|)) (SETUNION (IN-IMAGE (IN-IMAGE |f|) |w|)))))))))
)

(th~deftheorem X5205
 (in tps)
 (conclusion 
  (all-types a b (forall (lam (|w| (o (o b))) (forall (lam (|f| (a b)) (SUBSET (IN-IMAGE |f| (SETINTERSECT |w|)) (SETINTERSECT (IN-IMAGE (IN-IMAGE |f|) |w|)))))))))
)

(th~deftheorem X5206
 (in tps)
 (conclusion 
  (all-types b a (forall (lam (|f| (a b)) (forall (lam (|y| (o b)) (forall (lam (|x| (o b)) (= (IN-IMAGE |f| (UNION |x| |y|)) (UNION (IN-IMAGE |f| |x|) (IN-IMAGE |f| |y|)))))))))))
)

(th~deftheorem X5207
 (in tps)
 (conclusion 
  (all-types b a (forall (lam (|f| (a b)) (forall (lam (|y| (o b)) (forall (lam (|x| (o b)) (SUBSET (IN-IMAGE |f| (INTERSECT |x| |y|)) (INTERSECT (IN-IMAGE |f| |x|) (IN-IMAGE |f| |y|)))))))))))
)

(th~deftheorem X5208
 (in tps)
 (conclusion 
 (forall (lam (Q (o i)) (forall (lam (P (o i)) (equiv (exists (lam (S (o i)) (forall (lam (|x| i) (and (or (S |x|) (P |x|)) (or (not (S |x|)) (Q |x|))))))) (forall (lam (|y| i) (or (P |y|) (Q |y|))))))))))
)

(th~deftheorem X5209
 (in tps)
 (conclusion 
  (all-types a (forall (lam (E (o a)) (forall (lam (D (o a)) (= (POWERSET (INTERSECT D E)) (INTERSECT (POWERSET D) (POWERSET E)))))))))
)

(th~deftheorem X5210
 (in tps)
 (conclusion 
  (all-types a (forall (lam (|x| a) (= (= |x|) (lam (|z| a) (exists (lam (|y| a) (and (= |y| |x|) (= |z| |y|))))))))))
)

(th~deftheorem X5211
 (in tps)
 (conclusion 
  (all-types a (forall (lam (|y| (o a)) (= |y| (SETUNION (lam (|z| (o a)) (exists (lam (|x| a) (and (|y| |x|) (= |z| (= |x|))))))))))))
)

(th~deftheorem X5212
 (in tps)
 (conclusion 
  (all-types b a (forall (lam (|f| (a b)) (forall (lam (|g| (o b)) (= (lam (|z| a) (exists (lam (|x| b) (and (|g| |x|) (= |z| (|f| |x|)))))) (IN-IMAGE |f| |g|))))))))
)

(th~deftheorem X5303
 (in tps)
 (conclusion 
  (all-types a (= = (lam (|x| a) (lam (|y| a) (forall (lam (|p| (o a a)) (implies (forall (lam (|z| a) (|p| |z| |z|))) (|p| |x| |y|)))))))))
)

(th~deftheorem X5304
 (in tps)
 (conclusion 
  (all-types a (not (exists (lam (|g| (o a a)) (forall (lam (|f| (o a)) (exists (lam (|j| a) (= (|g| |j|) |f|))))))))))
)

(th~deftheorem X5305
 (in tps)
 (conclusion 
  (all-types a (forall (lam (|s| (o a)) (not (exists (lam (|g| (o a a)) (forall (lam (|f| (o a)) (implies (SUBSET |f| |s|) (exists (lam (|j| a) (and (|s| |j|) (= (|g| |j|) |f|))))))))))))))
)

(th~deftheorem X5308
 (in tps)
 (conclusion 
  (all-types a b (forall (lam (|r| (o b a)) (implies (exists (lam (|j| (b (o b))) (forall (lam (|p| (o b)) (implies (exists (lam (|x| b) (|p| |x|))) (|p| (|j| |p|))))))) (equiv (forall (lam (|x| a) (exists (lam (|y| b) (|r| |x| |y|))))) (exists (lam (|f| (b a)) (forall (lam (|x| a) (|r| |x| (|f| |x|))))))))))))
)

(th~deftheorem X5309
 (in tps)
 (conclusion 
 (not (exists (lam (|h| (i (o i))) (forall (lam (|p| (o i)) (forall (lam (|q| (o i)) (implies (= (|h| |p|) (|h| |q|)) (= |p| |q|))))))))))
)

(th~deftheorem X5310
 (in tps)
 (conclusion 
  (all-types b (implies (forall (lam (|r| (o b (o b))) (implies (forall (lam (|x| (o b)) (exists (lam (|y| b) (|r| |x| |y|))))) (exists (lam (|f| (b (o b))) (forall (lam (|x| (o b)) (|r| |x| (|f| |x|))))))))) (exists (lam (|j| (b (o b))) (forall (lam (|p| (o b)) (implies (exists (lam (|z| b) (|p| |z|))) (|p| (|j| |p|))))))))))
)

(th~deftheorem X5500
 (in tps)
 (conclusion 
  (all-types a b (forall (lam (J (b (o b))) (implies (forall (lam (P (o b)) (implies (exists (lam (|x| b) (P |x|))) (P (J P))))) (forall (lam (|f| (a b)) (forall (lam (|g| (a b)) (implies (= (|f| (J (lam (|x| b) (not (= (|f| |x|) (|g| |x|)))))) (|g| (J (lam (|x| b) (not (= (|f| |x|) (|g| |x|))))))) (= |f| |g|)))))))))))
)

(th~deftheorem X6004
 (in tps)
 (conclusion 
  (all-types b a (forall (lam (|y| a) (forall (lam (|x| b) (EQP (= |x|) (= |y|))))))))
)

(th~deftheorem X6101
 (in tps)
 (conclusion 
 (= ONE SIGMA1))
)

(th~deftheorem X6104
 (in tps)
 (conclusion 
  (all-types a (exists (lam (|i| (o (a a) (a a))) (and (forall (lam (|g| (a a)) (and (|i| |g| (lam (|x| a) |x|)) (|i| |g| (lam (|x| a) (|g| (|g| |x|))))))) (forall (lam (|f| (a a)) (forall (lam (|y| a) (implies (|i| (lam (|x| a) |y|) |f|) (= (|f| |y|) |y|)))))))))))
)

(th~deftheorem X6105
 (in tps)
 (conclusion 
 (forall (lam (|n| (o (o i))) (implies (NAT |n|) (forall (lam (|q| (o i)) (implies (|n| |q|) (exists (lam (|j| (i (o i))) (forall (lam (|r| (o i)) (implies (and (SUBSET |r| |q|) (exists (lam (|x| i) (|r| |x|)))) (|r| (|j| |r|))))))))))))))
 (help "This is a lemma for X6106. 
You may need to ASSERT DESCR or T5310 or T5310A"))

(th~deftheorem X6106
 (in tps)
 (conclusion 
 (implies (FINITE (lam (|x| i) true)) (exists (lam (|j| (i (o i))) (forall (lam (|r| (o i)) (implies (exists (lam (|x| i) (|r| |x|))) (|r| (|j| |r|)))))))))
)

(th~deftheorem X6201
 (in tps)
 (conclusion 
  (all-types a (implies (exists (lam (|r| (o a a)) (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|z| a) (and (and (exists (lam (|w| a) (|r| |x| |w|))) (not (|r| |x| |x|))) (implies (|r| |x| |y|) (implies (|r| |y| |z|) (|r| |x| |z|)))))))))))) (exists (lam (R (o (o a) (o a))) (forall (lam (X (o a)) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (and (and (exists (lam (W (o a)) (R X W))) (not (R X X))) (implies (R X Y) (implies (R Y Z) (R X Z)))))))))))))))
)

; Other items from the TPS library
(th~defdef CLOS-SYS2
 (in tps)
 (type-variables a b)
 (definition
(lam (CL (o (o b a))) (forall (lam (S (o (o b a))) (implies (SUBSET S CL) (CL (lam (|a| a) (lam (|b| b) (forall (lam (R (o b a)) (implies (S R) (R |a| |b|))))))))))))
 (help "closure system of binary relations"))

(th~defdef SUBRELN3
 (in tps)
 (type-variables a b c)
 (definition
(lam (R (o c b a)) (lam (S (o c b a)) (forall (lam (|a| a) (forall (lam (|b| b) (forall (lam (|c| c) (implies (R |a| |b| |c|) (S |a| |b| |c|)))))))))))
 (help "Subset for tertiary relations"))

(th~defdef LFP3
 (in tps)
 (type-variables a b c)
 (definition
(lam (CL (o (o c b a))) (lam (F (o c b a (o c b a))) (lam (|a| a) (lam (|b| b) (lam (|c| c) (forall (lam (R (o c b a)) (implies (and (CL R) (SUBRELN3 (F R) R)) (R |a| |b| |c|))))))))))
 (help "Least fixed point for a monotone operator on a closure system of tertiary relns."))

(th~defdef S-JOIN
 (in tps)
 (type-variables a)
 (definition
(lam (|0| a) (lam (P (a a a)) (LFP3 (lam (R (o a a a)) true) (lam (R (o a a a)) (lam (|x| a) (lam (|y| a) (lam (|z| a) (or (or (and (= |x| |0|) (= |y| |z|)) (and (= |y| |0|) (= |x| |z|))) (exists (lam (|x1| a) (exists (lam (|x2| a) (exists (lam (|y1| a) (exists (lam (|y2| a) (exists (lam (|z1| a) (exists (lam (|z2| a) (and (and (and (and (= |x| (P |x1| |x2|)) (= |y| (P |y1| |y2|))) (= |z| (P |z1| |z2|))) (R |x1| |y1| |z1|)) (R |x2| |y2| |z2|)))))))))))))))))))))))
 (help "Join relation for $ (not as a function to avoid descriptions)"))

(th~defdef S-INCL
 (in tps)
 (type-variables a)
 (definition
(lam (|0| a) (lam (P (a a a)) (lam (|x| a) (lam (|y| a) (S-JOIN |0| P |x| |y| |y|))))))
 (help "Inclusion for $"))

(th~defdef S-DUC
 (in tps)
 (type-variables a)
 (definition
(lam (|0| a) (lam (P (a a a)) (lam (D (o a)) (and (and (D |0|) (forall (lam (|x| a) (forall (lam (|y| a) (implies (and (D |y|) (S-INCL |0| P |x| |y|)) (D |x|))))))) (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|z| a) (implies (and (and (D |x|) (D |y|)) (S-JOIN |0| P |x| |y| |z|)) (D |z|)))))))))))))
 (help "nonempty subsets of $ that are downward closed and binary union (join) closed"))

(th~defdef INJECTIVE
 (in tps)
 (type-variables b a)
 (definition
(lam (|f| (a b)) (forall (lam (|x| b) (forall (lam (|y| b) (implies (= (|f| |x|) (|f| |y|)) (= |x| |y|))))))))
 (help ""))

(th~defdef SURJECTIVE
 (in tps)
 (type-variables b a)
 (definition
(lam (F (a b)) (forall (lam (Y a) (exists (lam (X b) (= (F X) Y)))))))
 (help ""))

(th~defdef CLOS-SYS1
 (in tps)
 (type-variables a)
 (definition
(lam (CL (o (o a))) (forall (lam (S (o (o a))) (implies (SUBSET S CL) (CL (SETINTERSECT S)))))))
 (help "Closure system of sets"))

(th~defdef TRANSITIVE
 (in tps)
 (type-variables a)
 (definition
(lam (R (o a a)) (forall (lam (X a) (forall (lam (Y a) (forall (lam (Z a) (implies (and (R X Y) (R Y Z)) (R X Z))))))))))
 (help "Transitivity"))

(th~defdef IND-TPS
 (in tps)
 (definition
(lam (|0| i) (lam (S (i i)) (forall (lam (|p| (o i)) (implies (and (|p| |0|) (forall (lam (|x| i) (implies (|p| |x|) (|p| (S |x|)))))) (forall (lam (|x| i) (|p| |x|)))))))))
 (help "Induction axiom"))

(th~defdef WELL-ORD
 (in tps)
 (type-variables a)
 (definition
(lam (R (o a a)) (forall (lam (X (o a)) (implies (exists (lam (|z| a) (X |z|))) (exists (lam (|z| a) (and (and (X |z|) (forall (lam (|x| a) (implies (X |x|) (R |z| |x|))))) (forall (lam (|y| a) (implies (and (X |y|) (forall (lam (|x| a) (implies (X |x|) (R |y| |x|))))) (= |y| |z|))))))))))))
 (help "Well-Ordering -- Defined here by saying every nonempty
set has a unique 'least' elt.  THM542 - THM546 show this implies
the relation is a linear order.  An alternative definition would
be to say the relation is antisymmetric and every nonempty set
has a 'least' elt. - Chad"))

(th~defdef REFLEXIVE
 (in tps)
 (type-variables a)
 (definition
(lam (R (o a a)) (forall (lam (X a) (R X X)))))
 (help ""))

(th~defdef SUBRELATION
 (in tps)
 (type-variables a b)
 (definition
(lam (|p| (o b a)) (lam (|r| (o b a)) (forall (lam (|x| a) (forall (lam (|y| b) (implies (|p| |x| |y|) (|r| |x| |y|)))))))))
 (help ""))

(th~defdef TCLOSED
 (in tps)
 (definition
(lam (|p| (o i i)) (lam (|s| (o i i)) (forall (lam (|u| i) (forall (lam (|v| i) (forall (lam (|w| i) (implies (and (|p| |u| |v|) (|s| |v| |w|)) (|p| |u| |w|)))))))))))
 (help "You can't get out of p by taking one step of s"))

(th~defdef ASSOCIATIVE
 (in tps)
 (type-variables a)
 (definition
(lam (P (a a a)) (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|z| a) (= (P (P |x| |y|) |z|) (P |x| (P |y| |z|)))))))))))
 (help ""))

(th~defdef IDEMPOTENT
 (in tps)
 (type-variables a)
 (definition
(lam (P (a a a)) (forall (lam (|x| a) (= (P |x| |x|) |x|)))))
 (help ""))

(th~defdef COMMUTATIVE
 (in tps)
 (type-variables b a)
 (definition
(lam (H (a b b)) (forall (lam (X b) (forall (lam (Y b) (= (H X Y) (H Y X))))))))
 (help ""))

(th~defdef LATTICE-LAW
 (in tps)
 (type-variables a)
 (definition
(lam (G (a a a)) (lam (H (a a a)) (forall (lam (|x| a) (forall (lam (|y| a) (= (G (H |x| |y|) |y|) |y|))))))))
 (help ""))

(th~defdef LATTICE
 (in tps)
 (type-variables a)
 (definition
(lam (JOIN (a a a)) (lam (MEET (a a a)) (and (and (and (and (and (and (and (IDEMPOTENT JOIN) (IDEMPOTENT MEET)) (ASSOCIATIVE JOIN)) (ASSOCIATIVE MEET)) (COMMUTATIVE JOIN)) (COMMUTATIVE MEET)) (LATTICE-LAW JOIN MEET)) (LATTICE-LAW MEET JOIN)))))
 (help "definition of a lattice (join ~ OR, meet ~ AND)"))

(th~defdef DISTRIBUTIVE
 (in tps)
 (type-variables a)
 (definition
(lam (G (a a a)) (lam (H (a a a)) (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|z| a) (= (G |x| (H |y| |z|)) (H (G |x| |y|) (G |x| |z|))))))))))))
 (help "One definition of distributivity in a lattice"))

(th~defdef MODULAR
 (in tps)
 (type-variables a)
 (definition
(lam (G (a a a)) (lam (H (a a a)) (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|z| a) (implies (= (H |x| |z|) |z|) (= (H |x| (G |y| |z|)) (G (H |x| |y|) |z|))))))))))))
 (help "Modularity (G = MEET / INTERSECTION, H = JOIN / UNION)"))

(th~defdef MODULAR-DEF2
 (in tps)
 (type-variables a)
 (definition
(lam (G (a a a)) (lam (H (a a a)) (forall (lam (|x| a) (forall (lam (|y| a) (forall (lam (|z| a) (= (H |x| (G |y| (H |x| |z|))) (G (H |x| |y|) (H |x| |z|))))))))))))
 (help "Another defn of modularity (G = MEET / INTERSECTION, H = JOIN / UNION)"))

(th~defdef DISTINCT-5
 (in tps)
 (type-variables a)
 (definition
(lam (|x| a) (lam (|y| a) (lam (|a| a) (lam (|b| a) (lam (|c| a) (and (and (and (and (and (and (and (and (and (not (= |a| |b|)) (not (= |a| |c|))) (not (= |a| |x|))) (not (= |a| |y|))) (not (= |b| |c|))) (not (= |b| |x|))) (not (= |b| |y|))) (not (= |c| |x|))) (not (= |c| |y|))) (not (= |x| |y|)))))))))
 (help "5 distinct objects of type a"))

(th~defdef PENTAGON
 (in tps)
 (type-variables a)
 (definition
(lam (G (a a a)) (lam (H (a a a)) (exists (lam (|x| a) (exists (lam (|y| a) (exists (lam (|a| a) (exists (lam (|b| a) (exists (lam (|c| a) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (DISTINCT-5 |x| |y| |a| |b| |c|) (= (G |x| |y|) |y|)) (= (H |x| |y|) |x|)) (= (G |x| |a|) |a|)) (= (H |x| |a|) |x|)) (= (G |x| |b|) |b|)) (= (H |x| |b|) |x|)) (= (G |x| |c|) |c|)) (= (H |x| |c|) |x|)) (= (G |a| |b|) |y|)) (= (H |a| |b|) |x|)) (= (G |a| |c|) |a|)) (= (H |a| |c|) |c|)) (= (G |a| |y|) |y|)) (= (H |a| |y|) |a|)) (= (G |b| |c|) |y|)) (= (H |b| |c|) |x|)) (= (G |b| |y|) |y|)) (= (H |b| |y|) |b|)) (= (G |c| |y|) |y|)) (= (H |c| |y|) |c|)))))))))))))))
 (help "a,b,c,x,y form a pentagon (G = MEET / INTERSECTION, H = JOIN / UNION, y = lowest, x = highest)"))

(th~defdef PLUS-INDEQS-TPS
 (in tps)
 (definition
(lam (PLUS-LIBCONST (i i i)) (lam (|0| i) (lam (S (i i)) (and (forall (lam (|n| i) (= (PLUS-LIBCONST |n| |0|) |n|))) (forall (lam (|n| i) (forall (lam (|m| i) (= (PLUS-LIBCONST |n| (S |m|)) (S (PLUS-LIBCONST |n| |m|))))))))))))
 (help "Inductive equations for +."))

(th~defdef 3-DIAMOND
 (in tps)
 (type-variables a)
 (definition
(lam (G (a a a)) (lam (H (a a a)) (exists (lam (|x| a) (exists (lam (|y| a) (exists (lam (|a| a) (exists (lam (|b| a) (exists (lam (|c| a) (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (DISTINCT-5 |x| |y| |a| |b| |c|) (= (G |x| |y|) |y|)) (= (H |x| |y|) |x|)) (= (G |x| |a|) |a|)) (= (H |x| |a|) |x|)) (= (G |x| |b|) |b|)) (= (H |x| |b|) |x|)) (= (G |x| |c|) |c|)) (= (H |x| |c|) |x|)) (= (G |a| |b|) |y|)) (= (H |a| |b|) |x|)) (= (G |a| |c|) |y|)) (= (H |a| |c|) |x|)) (= (G |a| |y|) |y|)) (= (H |a| |y|) |a|)) (= (G |b| |c|) |y|)) (= (H |b| |c|) |x|)) (= (G |b| |y|) |y|)) (= (H |b| |y|) |b|)) (= (G |c| |y|) |y|)) (= (H |c| |y|) |c|)))))))))))))))
 (help "a,b,c,x,y form a poset where a,b,c are all below x and above y but otherwise incomparable
(G = MEET / INTERSECTION, H = JOIN / UNION, y = lowest, x = highest)"))

(th~defdef TOP-ELEMENT
 (in tps)
 (type-variables a)
 (definition
(lam (JOIN (a a a)) (lam (MEET (a a a)) (lam (TOP a) (and (forall (lam (|x| a) (= (MEET TOP |x|) |x|))) (forall (lam (|x| a) (= (JOIN TOP |x|) TOP))))))))
 (help "TOP is the top element"))

(th~defdef BOTTOM-ELEMENT
 (in tps)
 (type-variables a)
 (definition
(lam (JOIN (a a a)) (lam (MEET (a a a)) (lam (BOTTOM a) (and (forall (lam (|x| a) (= (MEET BOTTOM |x|) BOTTOM))) (forall (lam (|x| a) (= (JOIN BOTTOM |x|) |x|))))))))
 (help "0(i) is the bottom element"))

(th~defdef LATTICE-COMP
 (in tps)
 (type-variables a)
 (definition
(lam (JOIN (a a a)) (lam (MEET (a a a)) (lam (TOP a) (lam (BOTTOM a) (lam (|x| a) (lam (|y| a) (and (= (JOIN |x| |y|) TOP) (= (MEET |x| |y|) BOTTOM)))))))))
 (help "Complementation in a lattice"))

(th~defdef COMPLEMENTED
 (in tps)
 (type-variables a)
 (definition
(lam (JOIN (a a a)) (lam (MEET (a a a)) (lam (TOP a) (lam (BOTTOM a) (forall (lam (|x| a) (exists (lam (|y| a) (LATTICE-COMP JOIN MEET TOP BOTTOM |x| |y|))))))))))
 (help "Complementation in a lattice"))

(th~defdef CD-LATTICE
 (in tps)
 (type-variables a)
 (definition
(lam (JOIN (a a a)) (lam (MEET (a a a)) (lam (TOP a) (lam (BOTTOM a) (and (and (and (and (and (LATTICE JOIN MEET) (DISTRIBUTIVE MEET JOIN)) (DISTRIBUTIVE JOIN MEET)) (TOP-ELEMENT JOIN MEET TOP)) (BOTTOM-ELEMENT JOIN MEET BOTTOM)) (COMPLEMENTED JOIN MEET TOP BOTTOM)))))))
 (help "Complemented distributive lattice"))

(th~defdef NBHD
 (in tps)
 (type-variables a)
 (definition
(lam (T (o (o a))) (lam (S (o a)) (lam (|x| a) (exists (lam (N (o a)) (and (and (T N) (SUBSET N S)) (N |x|))))))))
 (help "Given T a topology, S a set, x a point, NBHD T S x <=> S is a neighbourhood of x in T"))

(th~defdef DEDEKIND-CUT
 (in tps)
 (type-variables b)
 (definition
(lam (W (o b b)) (lam (C (o b)) (and (and (exists (lam (|q| b) (C |q|))) (exists (lam (|r| b) (not (C |r|))))) (forall (lam (|q| b) (forall (lam (|r| b) (implies (and (C |q|) (W |q| |r|)) (C |r|))))))))))
 (help "WRT a relation W(OBB) (thought of as an ordering), C(OB) is a
Dedekind cut if it is nonempty, not full, and upward closed.

NOTE(!!!):  I meant to define this with downward closed instead of
upward.  (The defn with downward closed is DDEDEKIND-CUT.)
The interpretation of the gwff's THM568, THM569 and THM570
are affected by this (in the sense that infs are actually sups, etc).
The corrected versions are THM571, THM572"))

(th~defdef GLB
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (lam (R (o a a)) (lam (S (o a)) (lam (|x| a) (and (and (P |x|) (forall (lam (|y| a) (implies (S |y|) (R |x| |y|))))) (forall (lam (|z| a) (implies (and (P |z|) (forall (lam (|y| a) (implies (S |y|) (R |z| |y|))))) (R |z| |x|))))))))))
 (help "Greatest Lower Bound -- this is essentially the same as the
abbr INF, but is relative to a set, not a type."))

(th~defdef GRP-ASSOC
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (forall (lam (|a| g) (forall (lam (|b| g) (forall (lam (|c| g) (= (|f| (|f| |a| |b|) |c|) (|f| |a| (|f| |b| |c|)))))))))))
 (help ""))

(th~defdef GRP-RIGHT-UNIT
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (forall (lam (|a| g) (= (|f| |a| |e|) |a|))))))
 (help ""))

(th~defdef GRP-RIGHT-INVERSE
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (forall (lam (|a| g) (exists (lam (|b| g) (= (|f| |a| |b|) |e|))))))))
 (help ""))

(th~defdef GROUP3
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (and (and (GRP-ASSOC |f|) (GRP-RIGHT-UNIT |f| |e|)) (GRP-RIGHT-INVERSE |f| |e|)))))
 (help ""))

(th~defdef GRP-LEFT-UNIT
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (forall (lam (|a| g) (= (|f| |e| |a|) |a|))))))
 (help ""))

(th~defdef GRP-LEFT-INVERSE
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (forall (lam (|a| g) (exists (lam (|b| g) (= (|f| |b| |a|) |e|))))))))
 (help ""))

(th~defdef GROUP2
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (and (and (GRP-ASSOC |f|) (GRP-LEFT-UNIT |f| |e|)) (GRP-LEFT-INVERSE |f| |e|)))))
 (help ""))

(th~defdef INDUCTION
 (in tps)
 (definition
(forall (lam (P (o (o (o i)))) (implies (and (P ZERO) (forall (lam (X (o (o i))) (implies (P X) (P (SUCC X)))))) (forall (lam (M (o (o i))) (implies (NAT M) (P M))))))))
 (help "Form of basic induction"))

(th~defdef COMPOSE
 (in tps)
 (type-variables g b a)
 (definition
(lam (|f| (a b)) (lam (|g| (b g)) (lam (|x| g) (|f| (|g| |x|))))))
 (help "Composition of functions"))

(th~defdef ITERATE+
 (in tps)
 (type-variables a)
 (definition
(lam (|f| (a a)) (lam (|g| (a a)) (forall (lam (|p| (o (a a))) (implies (and (|p| |f|) (forall (lam (|j| (a a)) (implies (|p| |j|) (|p| (COMPOSE |f| |j|)))))) (|p| |g|)))))))
 (help "[ITERATE+ f g] means that g is a composition of one or more copies of f"))

(th~defdef FIXPOINT
 (in tps)
 (definition
(lam (|g| (i i)) (lam (|x| i) (= (|g| |x|) |x|))))
 (help ""))

(th~defdef UNIQUE-FIXPOINT
 (in tps)
 (definition
(lam (|g| (i i)) (lam (|x| i) (and (FIXPOINT |g| |x|) (forall (lam (|z| i) (implies (FIXPOINT |g| |z|) (= |x| |z|))))))))
 (help ""))

(th~defdef ITERATE
 (in tps)
 (type-variables a)
 (definition
(lam (|f| (a a)) (lam (|g| (a a)) (forall (lam (|p| (o (a a))) (implies (and (|p| (lam (|u| a) |u|)) (forall (lam (|j| (a a)) (implies (|p| |j|) (|p| (COMPOSE |f| |j|)))))) (|p| |g|)))))))
 (help "[ITERATE f g] means that g can be obtained by composing f with itself zero or more times"))

(th~defdef SYMMETRIC
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (forall (lam (|x| a) (forall (lam (|y| a) (implies (|r| |x| |y|) (|r| |y| |x|))))))))
 (help ""))

(th~defdef EQUIVALENCE-REL
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (and (and (REFLEXIVE |r|) (SYMMETRIC |r|)) (TRANSITIVE |r|))))
 (help ""))

(th~defdef EQUIVALENCE-CLASSES
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|s| (o a)) (and (exists (lam (|z| a) (|s| |z|))) (forall (lam (|x| a) (implies (|s| |x|) (forall (lam (|y| a) (equiv (|s| |y|) (|r| |x| |y|)))))))))))
 (help ""))

(th~defdef PARTITION
 (in tps)
 (type-variables a)
 (definition
(lam (|s| (o (o a))) (and (forall (lam (|p| (o a)) (implies (|s| |p|) (exists (lam (|z| a) (|p| |z|)))))) (forall (lam (|x| a) (exists (lam (|p| (o a)) (and (and (|s| |p|) (|p| |x|)) (forall (lam (|q| (o a)) (implies (and (|s| |q|) (|q| |x|)) (= |q| |p|))))))))))))
 (help "S is a partition of the objects of type A"))

(th~defdef EQUIVALENCE-CLASSES-B
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|s| (o a)) (forall (lam (|x| a) (implies (|s| |x|) (forall (lam (|y| a) (equiv (|s| |y|) (|r| |x| |y|))))))))))
 (help "Assumes that R is an equivalence relation"))

(th~defdef PARTITION-B
 (in tps)
 (type-variables a)
 (definition
(lam (|s| (o (o a))) (forall (lam (|x| a) (exists (lam (|p| (o a)) (and (and (|s| |p|) (|p| |x|)) (forall (lam (|q| (o a)) (implies (and (|s| |q|) (|q| |x|)) (= |q| |p|)))))))))))
 (help "S is a partition of the objects of type A"))

(th~defdef PAIRUNION
 (in tps)
 (type-variables a)
 (definition
(lam (AA (o a a)) (lam (BB (o a a)) (lam (|xx| a) (lam (|yy| a) (or (AA |xx| |yy|) (BB |xx| |yy|)))))))
 (help "union of a set of pairs"))

(th~defdef TRCL
 (in tps)
 (type-variables a)
 (definition
(lam (|r1| (o a a)) (lam (|x1| a) (lam (|y1| a) (forall (lam (|p1| (o a a)) (implies (and (SUBRELATION |r1| |p1|) (TRANSITIVE |p1|)) (|p1| |x1| |y1|))))))))
 (help ""))

(th~defdef ADD1
 (in tps)
 (type-variables a)
 (definition
(lam (|p| (o a)) (lam (|x| a) (lam (|t| a) (or (|p| |t|) (= |t| |x|))))))
 (help "Operator which adds one element to a set"))

(th~defdef FINITE-SET-SUBSET-EXT
 (in tps)
 (type-variables a)
 (definition
(lam (X (o a)) (forall (lam (P (o (o a))) (implies (and (forall (lam (E (o a)) (implies (not (exists (lam (|t| a) (E |t|)))) (P E)))) (forall (lam (Y (o a)) (forall (lam (|x| a) (forall (lam (Z (o a)) (implies (and (P Y) (SUBSET Z (ADD1 Y |x|))) (P Z))))))))) (P X))))))
 (help "Definition of finite set that builds in extensionality using subsets.
Compare this to the definition of FINITE1."))

(th~defdef GRP-UNIT
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (forall (lam (|a| g) (and (= (|f| |e| |a|) |a|) (= (|f| |a| |e|) |a|)))))))
 (help ""))

(th~defdef GRP-INVERSE
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (forall (lam (|a| g) (exists (lam (|b| g) (and (= (|f| |a| |b|) |e|) (= (|f| |b| |a|) |e|)))))))))
 (help ""))

(th~defdef GROUP1
 (in tps)
 (type-variables g)
 (definition
(lam (|f| (g g g)) (lam (|e| g) (and (and (GRP-ASSOC |f|) (GRP-UNIT |f| |e|)) (GRP-INVERSE |f| |e|)))))
 (help ""))

(th~defdef ITERATES
 (in tps)
 (type-variables a)
 (definition
(lam (G (a a)) (lam (F (a a)) (forall (lam (P (o (a a))) (implies (and (P F) (forall (lam (H (a a)) (implies (P H) (P (lam (T a) (F (H T)))))))) (P G)))))))
 (help ""))

(th~defdef REL-PROP-CLOSURE
 (in tps)
 (type-variables a b)
 (definition
(lam (|h| (o (o b a))) (lam (|r| (o b a)) (lam (|x| a) (lam (|y| b) (forall (lam (|p| (o b a)) (implies (and (SUBRELATION |r| |p|) (|h| |p|)) (|p| |x| |y|)))))))))
 (help "The REL-PROP-CLOSURE of a relation r 
under a property h is the intersection of all relations p which
include r and have the property h."))

(th~defdef MAPS2
 (in tps)
 (type-variables g b a)
 (definition
(lam (|k| (a b g)) (lam (|u| (o g)) (lam (|v| (o b)) (lam (|z| (o a)) (forall (lam (|x| g) (forall (lam (|y| b) (implies (and (|u| |x|) (|v| |y|)) (|z| (|k| |x| |y|))))))))))))
 (help "[maps2 k u v z] means that the binary function k maps uXv into z."))

(th~defdef -CLOSED2
 (in tps)
 (type-variables a)
 (definition
(lam (|k| (a a a)) (lam (|u| (o a)) (MAPS2 |k| |u| |u| |u|))))
 (help "[k -closed2 u] means that the set u is closed under the binary function k."))

(th~defdef MAPS
 (in tps)
 (type-variables b a)
 (definition
(lam (|k| (a b)) (lam (|u| (o b)) (lam (|v| (o a)) (forall (lam (|x| b) (implies (|u| |x|) (|v| (|k| |x|)))))))))
 (help "[maps k u v] means that the function k maps the set u into the set v."))

(th~defdef HOM2
 (in tps)
 (type-variables b a)
 (definition
(lam (|k| (a b)) (lam (|r| (o b)) (lam (|f| (b b b)) (lam (|s| (o a)) (lam (|g| (a a a)) (and (and (and (-CLOSED2 |f| |r|) (-CLOSED2 |g| |s|)) (MAPS |k| |r| |s|)) (forall (lam (|x| b) (forall (lam (|y| b) (implies (and (|r| |x|) (|r| |y|)) (= (|k| (|f| |x| |y|)) (|g| (|k| |x|) (|k| |y|)))))))))))))))
 (help "[hom2 k r f s g] means that k is a homomorphism from <r,f> to <s,g>, where f is a binary operator on r and g is a binary operator on s."))

(th~defdef RCLOSURE
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|x| a) (lam (|y| a) (or (|r| |x| |y|) (= |x| |y|))))))
 (help "Reflexive closure of a relation"))

(th~defdef CLOSURE
 (in tps)
 (type-variables a)
 (definition
(lam (|property| (o (o a a))) (lam (|r| (o a a)) (lam (|x| a) (lam (|y| a) (forall (lam (|p| (o a a)) (implies (and (SUBRELATION |r| |p|) (|property| |p|)) (|p| |x| |y|)))))))))
 (help "The closure of a relation R under a property is the intersection of 
all the relations P containing R and having the property"))

(th~defdef -CLOSED
 (in tps)
 (type-variables a)
 (definition
(lam (|k| (a a)) (lam (|u| (o a)) (MAPS |k| |u| |u|))))
 (help "[k -closed u] means that the set u is closed under k."))

(th~defdef HOM
 (in tps)
 (type-variables b a)
 (definition
(lam (|k| (a b)) (lam (|r| (o b)) (lam (|f| (b b)) (lam (|s| (o a)) (lam (|g| (a a)) (and (and (and (-CLOSED |f| |r|) (-CLOSED |g| |s|)) (MAPS |k| |r| |s|)) (forall (lam (|x| b) (implies (|r| |x|) (= (|k| (|f| |x|)) (|g| (|k| |x|)))))))))))))
 (help "[hom k r f s g] means that k is a homomorphism from <r,f> to <s,g>, where f is an operator on r and g is an operator on s."))

(th~defdef EQUIVS1
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (lam (R (o a)) (forall (lam (|x| a) (= (P |x|) (R |x|)))))))
 (help "equivs without EQUIV"))

(th~defdef MONOTONE3
 (in tps)
 (type-variables a b c)
 (definition
(lam (CL (o (o c b a))) (lam (F (o c b a (o c b a))) (and (forall (lam (R (o c b a)) (implies (CL R) (CL (F R))))) (forall (lam (R (o c b a)) (forall (lam (S (o c b a)) (implies (and (and (CL R) (CL S)) (SUBRELN3 R S)) (SUBRELN3 (F R) (F S)))))))))))
 (help "Monotone function on a class of tertiary relations"))

(th~defdef SCLOSURE
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|x| a) (lam (|y| a) (or (|r| |x| |y|) (|r| |y| |x|))))))
 (help "Symmetric closure of a relation"))

(th~defdef DDEDEKIND-CUT
 (in tps)
 (type-variables b)
 (definition
(lam (W (o b b)) (lam (C (o b)) (and (and (exists (lam (|q| b) (C |q|))) (exists (lam (|r| b) (not (C |r|))))) (forall (lam (|q| b) (forall (lam (|r| b) (implies (and (C |r|) (W |q| |r|)) (C |q|))))))))))
 (help "WRT a relation W(OBB) (thought of as an ordering), C(OB) is a
Dedekind cut if it is nonempty, not full, and downward closed."))

(th~defdef PAIRSETUNION
 (in tps)
 (type-variables a)
 (definition
(lam (F (o (o a a))) (lam (|x| a) (lam (|y| a) (exists (lam (R (o a a)) (R |x| |y|)))))))
 (help "set union for relations"))

(th~defdef TC2
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|x| a) (lam (|y| a) (forall (lam (|q| (o a)) (implies (and (forall (lam (|w| a) (implies (|r| |x| |w|) (|q| |w|)))) (forall (lam (|u| a) (forall (lam (|v| a) (implies (and (|q| |u|) (|r| |u| |v|)) (|q| |v|))))))) (|q| |y|))))))))
 (help "Alternative definition of transitive closure of a relation"))

(th~defdef NULLSET
 (in tps)
 (type-variables a)
 (definition
(lam (|x| a) false))
 (help ""))

(th~defdef SETDIFF
 (in tps)
 (type-variables a)
 (definition
(lam (|u| (o a)) (lam (|v| (o a)) (lam (|x| a) (and (|u| |x|) (not (|v| |x|)))))))
 (help "set difference"))

(th~defdef SIGMA1A
 (in tps)
 (type-variables a)
 (definition
(lam (|p| (o a)) (exists (lam (|y| a) (and (|p| |y|) (forall (lam (|z| a) (implies (|p| |z|) (= |y| |z|)))))))))
 (help "Another predicate asserting that a
set is a unit set. Related to SIGMA1. See 5306 in TTTP."))

(th~defdef S-ALG
 (in tps)
 (type-variables $)
 (definition
(lam (|0| $) (lam (P ($ $ $)) (and (and (forall (lam (|x| $) (forall (lam (|y| $) (not (= (P |x| |y|) |0|)))))) (forall (lam (|x| $) (forall (lam (|y| $) (forall (lam (|u| $) (forall (lam (|v| $) (implies (= (P |x| |u|) (P |y| |v|)) (and (= |x| |y|) (= |u| |v|)))))))))))) (forall (lam (X (o $)) (implies (and (X |0|) (forall (lam (|x| $) (forall (lam (|y| $) (implies (and (X |x|) (X |y|)) (X (P |x| |y|)))))))) (forall (lam (|x| $) (X |x|))))))))))
 (help "$"))

(th~defdef S-JOIN-CLOS
 (in tps)
 (type-variables $)
 (definition
(lam (|0| $) (lam (P ($ $ $)) (lam (JOIN (o $ $ $)) (and (and (forall (lam (|x| $) (JOIN |x| |0| |x|))) (forall (lam (|y| $) (JOIN |0| |y| |y|)))) (forall (lam (|x| $) (forall (lam (|y| $) (forall (lam (|z| $) (forall (lam (|u| $) (forall (lam (|v| $) (forall (lam (|w| $) (implies (and (JOIN |x| |y| |z|) (JOIN |u| |v| |w|)) (JOIN (P |x| |u|) (P |y| |v|) (P |z| |w|))))))))))))))))))))
 (help "The closure conditions of the JOIN relation on the initial pairing algebra $.
SEE ALSO: S-ALG, S-JOIN-IND."))

(th~defdef TC-CLOSED
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|x| (o a)) (forall (lam (|y| a) (forall (lam (|z| a) (implies (and (|r| |y| |z|) (|x| |y|)) (|x| |z|)))))))))
 (help ""))

(th~defdef TRCL-BBP
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|a| a) (lam (|b| a) (forall (lam (|x| (o a)) (implies (TC-CLOSED |r| |x|) (implies (|x| |a|) (|x| |b|)))))))))
 (help "Bailin & Barker-Plummer's defn of trcl"))

(th~defdef TOPOLOGY
 (in tps)
 (type-variables a)
 (definition
(lam (X (o (o a))) (and (and (and (forall (lam (R (o a)) (implies (= R (lam (|x| a) false)) (X R)))) (forall (lam (R (o a)) (implies (= R (lam (|x| a) (not false))) (X R))))) (forall (lam (K (o (o a))) (forall (lam (R (o a)) (implies (and (SUBSET K X) (= R (SETUNION K))) (X R))))))) (forall (lam (Y (o a)) (forall (lam (Z (o a)) (forall (lam (S (o a)) (implies (and (and (X Y) (X Z)) (= S (INTERSECT Y Z))) (X S)))))))))))
 (help ""))

(th~defdef CLOSED
 (in tps)
 (type-variables a)
 (definition
(lam (T (o (o a))) (lam (S (o a)) (forall (lam (R (o a)) (implies (= R (COMPLEMENT-TPS S)) (T R)))))))
 (help "Assumes T is a TOPOLOGY"))

(th~defdef CONTINUOUS
 (in tps)
 (type-variables a b)
 (definition
(lam (S (o (o b))) (lam (T (o (o a))) (lam (|f| (b a)) (forall (lam (X (o b)) (implies (S X) (forall (lam (Y (o a)) (implies (= Y (lam (|b| a) (X (|f| |b|)))) (T Y)))))))))))
 (help "f is a continuous function from T to S (where T and S are topological spaces)"))

(th~defdef PAIR1
 (in tps)
 (type-variables a)
 (definition
(lam (|x| a) (lam (|y| a) (lam (|g| (a a a)) (|g| |x| |y|)))))
 (help "Ordered pair of objects of the same type"))

(th~defdef SETPAIRSINTERSECT
 (in tps)
 (type-variables a b)
 (definition
(lam (|s| (o (o b a))) (lam (|x| a) (lam (|y| b) (forall (lam (|p| (o b a)) (implies (|s| |p|) (|p| |x| |y|))))))))
 (help "Intersection of a set of binary relations"))

(th~defdef ANTISYMMETRIC
 (in tps)
 (type-variables a)
 (definition
(lam (R (o a a)) (forall (lam (|x| a) (forall (lam (|y| a) (implies (and (R |x| |y|) (R |y| |x|)) (= |x| |y|))))))))
 (help ""))

(th~defdef PAR-ORD
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a a)) (and (and (TRANSITIVE P) (REFLEXIVE P)) (ANTISYMMETRIC P))))
 (help "Partial Order.  Hard to believe this is not defined elsewhere."))

(th~defdef LIN-ORD
 (in tps)
 (type-variables a)
 (definition
(lam (R (o a a)) (and (PAR-ORD R) (forall (lam (|x| a) (forall (lam (|y| a) (or (R |x| |y|) (R |y| |x|)))))))))
 (help "Linear Order"))

(th~defdef DOWN-CLOSED
 (in tps)
 (type-variables a)
 (definition
(lam (R (o a a)) (lam (S (o a)) (forall (lam (|u| a) (forall (lam (|v| a) (implies (and (R |u| |v|) (S |v|)) (S |u|)))))))))
 (help "Downward closed set wrt a relation R"))

(th~defdef FINITESET
 (in tps)
 (type-variables a)
 (definition
(lam (|e| (o a)) (forall (lam (X (o (o a))) (implies (and (X (lam (|y| a) false)) (forall (lam (|x| (o a)) (implies (X |x|) (forall (lam (|t| a) (implies (|e| |t|) (X (UNION |x| (UNITSET |t|)))))))))) (X |e|))))))
 (help "Predicate for finite sets without using natural numbers."))

(th~defdef SETPAIR
 (in tps)
 (type-variables a)
 (definition
(lam (|x| a) (lam (|y| a) (lam (|v| a) (or (= |v| |x|) (= |v| |y|))))))
 (help "Takes a pair of arguments of type A
and returns a set (type (OA)) containing
the two arguments"))

(th~defdef FINITE1C
 (in tps)
 (type-variables c)
 (definition
(forall (lam (F (c c)) (implies (INJECTIVE F) (SURJECTIVE F)))))
 (help "Was called FINITE1 in TPS2, where C was a type variable"))

(th~defdef FINITE1B
 (in tps)
 (type-variables b)
 (definition
(forall (lam (F (b b)) (implies (INJECTIVE F) (SURJECTIVE F)))))
 (help "Was called FINITE1 in TPS2, where B was a type variable"))

(th~defdef ADDPAIR
 (in tps)
 (type-variables a b)
 (definition
(lam (|k| (o b a)) (lam (|x| a) (lam (|y| b) (lam (|u| a) (lam (|v| b) (or (|k| |u| |v|) (and (= |u| |x|) (= |v| |y|)))))))))
 (help "[ADDPAIR k x y] is the result of adding
the ordered pair <x,y> to the set k of ordered pairs."))

(th~defdef SUBTRACTPAIR
 (in tps)
 (type-variables b a)
 (definition
(lam (|k| (o a b)) (lam (|x| b) (lam (|y| a) (lam (|u| b) (lam (|v| a) (and (|k| |u| |v|) (not (and (= |u| |x|) (= |v| |y|))))))))))
 (help "[SUBTRACTPAIR k x y] is the result of deleting 
the ordered pair <x,y> from the set k of ordered pairs."))

(th~defdef GEN-HOM-TPS
 (in tps)
 (type-variables a b)
 (definition
(lam (* (b b b)) (lam (*^0 (a a a)) (lam (|f| (a b)) (forall (lam (|x| b) (forall (lam (|y| b) (= (|f| (* |x| |y|)) (*^0 (|f| |x|) (|f| |y|)))))))))))
 (help "generic homomorphism (* is left undefined)"))

(th~defdef P-REC-TPS
 (in tps)
 (definition
(lam (|0| (o (o i))) (lam (S ((o (o i)) (o (o i)))) (lam (H ((o (o i)) (o (o i)) (o (o i)))) (lam (G (o (o i))) (lam (N (o (o i))) (that (lam (M (o (o i))) (forall (lam (W (o (o (o i)) (o (o i)))) (implies (and (W |0| G) (forall (lam (X (o (o i))) (forall (lam (Y (o (o i))) (implies (W X Y) (W (S X) (H X Y)))))))) (W N M))))))))))))
 (help "Primitive Recursion operator. Needs O and S to be replaced by ZERO and SUCC."))

(th~defdef PLUS-TPS
 (in tps)
 (definition
(lam (\|0\| (o (o i))) (lam (S ((o (o i)) (o (o i)))) (P-REC-TPS \|0\| S (lam (X (o (o i))) (lam (Y (o (o i))) (S Y)))))))
 (help "Addition defined primitive recursively. Uses P-REC, which has ) and S free."))

(th~defdef PA-1-TPS
 (in tps)
 (type-variables n)
 (definition
(lam (+ (n n n)) (lam (|0| n) (forall (lam (|x| n) (= (+ |x| |0|) |x|))))))
 (help ""))

(th~defdef PA-2-TPS
 (in tps)
 (type-variables n)
 (definition
(lam (+ (n n n)) (lam (S (n n)) (forall (lam (|x| n) (forall (lam (|y| n) (= (+ |x| (S |y|)) (S (+ |x| |y|))))))))))
 (help ""))

(th~defdef PA-IND-EQ-TPS
 (in tps)
 (type-variables n)
 (definition
(lam (|0| n) (lam (S (n n)) (forall (lam (|p| (n n)) (forall (lam (|q| (n n)) (implies (and (= (|p| |0|) (|q| |0|)) (forall (lam (|x| n) (implies (= (|p| |x|) (|q| |x|)) (= (|p| (S |x|)) (|q| (S |x|))))))) (forall (lam (|x| n) (= (|p| |x|) (|q| |x|))))))))))))
 (help ""))

(th~defdef LESS-INFIX-TPS
 (in tps)
 (type-variables b a)
 (definition
(lam (< (o a b)) <))
 (help "This just allows the predicate < to be printed in infix position"))

(th~defdef EXISTS1B
 (in tps)
 (type-variables a)
 (definition
(lam (P (o a)) (exists (lam (X a) (and (P X) (forall (lam (Y a) (implies (P Y) (= X Y)))))))))
 (help ""))

(th~defdef IDIAG
 (in tps)
 (definition
(lam (|h| (i (o i))) (IN-IMAGE |h| (lam (|s| (o i)) (not (|s| (|h| |s|)))))))
 (help "Crucial substitution for the Injective Cantor Theorem (X5309)"))

(th~defdef TRANSITIVE-CLOSURE
 (in tps)
 (type-variables a)
 (definition
(lam (|r| (o a a)) (lam (|x| a) (lam (|y| a) (forall (lam (|p| (o a a)) (implies (and (SUBRELATION |r| |p|) (TRANSITIVE |p|)) (|p| |x| |y|))))))))
 (help ""))

(th~defdef HOMOM2
 (in tps)
 (type-variables b a)
 (definition
(lam (|k| (a b)) (lam (|f| (b b b)) (lam (|g| (a a a)) (forall (lam (|x| b) (forall (lam (|y| b) (= (|k| (|f| |x| |y|)) (|g| (|k| |x|) (|k| |y|)))))))))))
 (help "[homom2 k f g] means that k is a homomorphism from things of type B to things of type A, where f and g are binary operators."))

(th~defdef FINITE1
 (in tps)
 (type-variables a)
 (definition
(lam (|p| (o a)) (forall (lam (|w| (o (o a))) (implies (and (|w| (lam (|x| a) false)) (forall (lam (|r| (o a)) (forall (lam (|x| a) (implies (|w| |r|) (|w| (ADD1 |r| |x|)))))))) (|w| |p|))))))
 (help "An elegant definition of finiteness"))

(th~defdef EQP1
 (in tps)
 (type-variables a b)
 (definition
(lam (|p| (o b)) (lam (|q| (o a)) (exists (lam (|s| (a b)) (and (forall (lam (|x| b) (implies (|p| |x|) (|q| (|s| |x|))))) (forall (lam (|y| a) (implies (|q| |y|) (exists (lam (|x| b) (and (and (|p| |x|) (= |y| (|s| |x|))) (forall (lam (|z| b) (implies (and (|p| |z|) (= |y| (|s| |z|))) (= |z| |x|))))))))))))))))
 (help "Equipotence without using EXISTS1"))

(th~defdef TOP-CLOSURE
 (in tps)
 (type-variables a)
 (definition
(lam (T (o (o a))) (lam (S (o a)) (SETINTERSECT (lam (W (o a)) (and (SUBSET S W) (CLOSED T W)))))))
 (help "Assumes T is a TOPOLOGY"))

(th~defdef IN
 (in tps)
 (type-variables a)
 (definition
(lam (|x| a) (lam (|p| (o a)) (|p| |x|))))
 (help "membership relation"))

(th~defdef LIKES-INFIX-TPS
 (in tps)
 (type-variables b a)
 (definition
(lam (LIKES (o a b)) LIKES))
 (help "This just allows the predicate LIKES to be printed in infix position"))

(th~defdef LC-RELN
 (in tps)
 (type-variables a)
 (definition
(lam (R (o a a)) (and (REFLEXIVE R) (forall (lam (|u| a) (forall (lam (|v| a) (forall (lam (|w| a) (implies (and (R |u| |w|) (R |v| |w|)) (R |u| |v|)))))))))))
 (help "A relation that is reflexive and satisfies u R w and v R w implies u R v.
This is a generalization of an equivalence relation (as the second property is
implied by symmetry and transitivity).  I'm calling this a 'left congruence relation',
though this is probably a terribly misleading name."))

(th~defdef LEFT1
 (in tps)
 (type-variables a)
 (definition
(lam (|p| (a (a a a))) (|p| (lam (|x| a) (lam (|y| a) |x|)))))
 (help "Left component of an ordered pair of objects of the same type"))

(th~defdef RIGHT1
 (in tps)
 (type-variables a)
 (definition
(lam (|p| (a (a a a))) (|p| (lam (|x| a) (lam (|y| a) |y|)))))
 (help "Right component of an ordered pair of objects of the same type"))

(th~defdef APR1
 (in tps)
 (type-variables a b)
 (definition
(lam (|h| (b a a)) (lam (|p| (a (a a a))) (|h| (LEFT1 |p|) (RIGHT1 |p|)))))
 (help "[APR1 h p] is the result of Applying h to the PaiR p"))

(th~defdef INREL1
 (in tps)
 (type-variables a)
 (definition
(lam (|p| (a (a a a))) (lam (|r| (o a a)) (|r| (LEFT1 |p|) (RIGHT1 |p|)))))
 (help "INREL1 p r means that the ordered pair p 
(with components of the same type) is in the binary relation r"))

(th~defdef ISPAIR1
 (in tps)
 (type-variables a)
 (definition
(lam (|p| (a (a a a))) (= |p| (PAIR1 (LEFT1 |p|) (RIGHT1 |p|)))))
 (help "[IsPair1] means that p is an ordered pair."))

(th~defdef RANGE
 (in tps)
 (type-variables b a)
 (definition
(lam (|f| (a b)) (lam (|y| a) (exists (lam (|x| b) (= (|f| |x|) |y|))))))
 (help "the range of the function"))

(th~defdef STRANGE-HO-ABBR
 (in tps)
 (definition
(lam (S (o (o i i))) (lam (|x| i) (lam (|y| i) (forall (lam (|p| (o i i)) (implies (and (S |p|) (|p| |x| |y|)) (|p| |y| |x|))))))))
 (help ""))

(th~defdef DIFF
 (in tps)
 (type-variables a)
 (definition
(lam (T (o a)) (lam (S (o a)) (lam (|x| a) (and (T |x|) (not (S |x|)))))))
 (help "The difference of T and S"))

(th~defdef SYMDIFF
 (in tps)
 (type-variables a)
 (definition
(lam (T (o a)) (lam (S (o a)) (UNION (DIFF T S) (DIFF S T)))))
 (help "The symmetric difference of T and S"))

(th~defdef MEETS
 (in tps)
 (type-variables a)
 (definition
(lam (X (o a)) (lam (Y (o a)) (exists (lam (|x| a) (and (X |x|) (Y |x|)))))))
 (help "Non-empty intersection"))

(th~defdef MISSES
 (in tps)
 (type-variables a)
 (definition
(lam (X (o a)) (lam (Y (o a)) (not (MEETS X Y)))))
 (help "Empty intersection"))

